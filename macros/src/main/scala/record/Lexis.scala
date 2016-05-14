package record

import com.mongodb.{BasicDBList, BasicDBObject, DBObject, BasicDBObjectBuilder}
import scala.reflect.runtime.universe._

/**
 * Created by tim on 29.04.16.
 */
trait Lexis {

  type M = Meta[_]
  private val setExpression = classOf[SetExpression[_,_]]
  private val minExpression = classOf[MinExpression[_]]
  private val maxExpression = classOf[MaxExpression[_]]
  private val renameExpression = classOf[RenameExpression[_,_]]
  private val incExpression = classOf[IncExpression[_,_]]
  private val mulExpression = classOf[MulExpression[_,_]]
  private val unsetExpression = classOf[UnsetExpression[_,_]]

  trait Make[C]

  trait Meta[C] extends Make[C] {
    val collection_name:String
    override def toString:String = collection_name
    def insert(c: C)(implicit ev1: TypeTag[C]):InsertResult[C] = InsertResult(this, DBObjectSerializer.asDBObjectImplicit(c)(ev1))
    def isValid(c: C):Boolean = true
    def apply(c1: this.type => SelectExpression)(implicit ev1: TypeTag[C]): SelectResult[this.type] = SelectResult(this, c1(this), typeOf[C])
    def copy(collection_name: String = this.collection_name):Meta[C] =  new Meta[C] {
      override val collection_name: String = collection_name
    }
  }

  case class SelectResult[T <: M](c: T, s: SelectExpression, tag: Type) extends Query {
    override def toString: String = MongoBuilder.buildSelectResultAsString(this)

    override def execute: execute = MongoBuilder.builderSelectResult(this)
  }

  case class JoinResult[T <: M, T1 <: M](c: T, c1: T1, joined: Join[_, _, _]) extends Query {
    override def toString: String = MongoBuilder.buildJoinResultAsString(this)

    override def execute: execute = ???
  }

  trait SelectExpression {
    val w: Expression[_]
  }

  case class SelectEntity[T <: M](w: Expression[_], c: T) extends SelectExpression

  case class SelectFields[C](w: Expression[_], c: Seq[Field[C, _]]) extends SelectExpression

  case class UpdateResult[T <: M](c: T, s: Update[_]) extends Query {
    override def toString: String = MongoBuilder.buildUpdateResultAsString(this)

    override def execute: execute = MongoBuilder.builderUpdateResult(this)
  }

  case class WhereExpression[C](c: Expression[C]) extends Update[C] {
    val parent = null
    val left = null
    val right = null
    val symbol: String = null
    def select[S <: Meta[C]](c1: S) = {
      SelectEntity(c, c1)
    }

    def select(c1: Field[C, _]*) = {
      SelectFields(c, c1)
    }

    def on[C1, F](f: => Join[C, C1, F]): Join[C, C1, F] = f

  }

  trait Expression[T] {
    def &&(r: Expression[T]) = LogicalExpression(this, r, "$and")

    def ||(r: Expression[T]) = LogicalExpression(this, r, "$or")
  }

  /**
   *
   * @param left
   * @param right
   * @param operator
   * @tparam C collection
   * @tparam F field
   */
  case class BooleanExpression[C, F](left: Field[C, F], right: F, operator: String)(implicit ev1: TypeTag[F]) extends Expression[C] {
    def getRight:Any = DBObjectSerializer.asDBObjectImplicit(right)(ev1)
  }

  case class LogicalExpression[C](left: Expression[C], right: Expression[C], operator: String) extends Expression[C]

  case class Join[C, C1, F](owner: Field[C, F], joined: Field[C1, F], stack: Seq[Join[_, _, _]]) {
    def on[C2](f: => Join[C, C2, F]) = this.copy(stack = stack :+ f)
  }

  case class InsertResult[C](t: Meta[C], c: Any) extends Query {
    override def toString: String = MongoBuilder.buildInsertResultAsString(this)

    override def execute: execute = ???
  }

  trait Update[C] {
    val parent:Update[C]
    val left: Field[C, _]
    val right: Any
    override def toString:String = s"${left.toString} : '${right.toString}'"
    def set[F](left: Field[C, F], right: F):Update[C] = SetExpression(this, left, right)
    def unset(left: Field[C, _]):Update[C] = UnsetExpression(this, left)
    def inc(left: Field[C, Int], right: Int):Update[C] = IncExpression(this, left, right)
    def mul(left: Field[C, Int], right: Int):Update[C] = MulExpression(this, left, right)
    def rename[F](left: Field[C, F], right: String):Update[C] = RenameExpression(this, left, right)
    def min(left: Field[C, Int], right: Int):Update[C] = MinExpression(this, left, right)
    def max(left: Field[C, Int], right: Int):Update[C] = MaxExpression(this, left, right)
    def flatten:Map[Class[_ <: Update[_]], List[Update[C]]] = {

      val parents = scala.collection.mutable.ArrayBuffer[Update[C]](this)
      var p = this

      while (p.parent != null) {
        parents += p.parent
        p = p.parent
      }
      parents.toList.groupBy(_.getClass).toArray.sortBy(_._1.getClass.getName).toMap
    }
    def condition = flatten(classOf[WhereExpression[C]]).head.asInstanceOf[WhereExpression[C]]
    def modify = flatten.filter( _._1 != classOf[WhereExpression[_]])
  }

  case class SetExpression[C, F](parent: Update[C], left: Field[C, F], right: F) extends Update[C]

  case class IncExpression[C, F](parent: Update[C], left: Field[C, F], right: F) extends Update[C]

  case class MulExpression[C, F](parent: Update[C], left: Field[C, F], right: F) extends Update[C]

  case class RenameExpression[C, F](parent: Update[C], left: Field[C, F], right: String) extends Update[C]

  case class UnsetExpression[C, F](parent: Update[C], left: Field[C, F]) extends Update[C] {
    override val right = 1
  }

  case class MinExpression[C](parent: Update[C], left: Field[C, Int], right: Int) extends Update[C]

  case class MaxExpression[C](parent: Update[C], left: Field[C, Int], right: Int) extends Update[C]

  /**
   *
   * @tparam C collection
   * @tparam F field
   */
  // implicit where
  trait Field[C, F] extends Make[C] {

    val fieldName: String
    val collection: Make[C]

    def ===(right: F)(implicit ev1: TypeTag[F]) = BooleanExpression(this, right, "$eq")(ev1)

    def ===[C1](joined: Field[C1, F]) = Join(this, joined, Seq())

    def >(right: F)(implicit ev1: TypeTag[F]) = BooleanExpression(this, right, "$gt")(ev1)

    def <(right: F)(implicit ev1: TypeTag[F]) = BooleanExpression(this, right, "$lt")(ev1)

    def >=(right: F)(implicit ev1: TypeTag[F]) = BooleanExpression(this, right, "$gte")(ev1)

    def <=(right: F)(implicit ev1: TypeTag[F]) = BooleanExpression(this, right, "$lte")(ev1)

    override def toString: String = {
      collection match {
        case x: Meta[C] => fieldName
        case x: Field[C, _] => x+"."+fieldName
      }
    }

  }

  case class UField[C, F](fieldName: String, collection: Make[C]) extends Field[C, F]
  case class StringField[C](fieldName: String, collection: Make[C]) extends Field[C, String]
  case class IntField[C](fieldName: String, collection: Make[C]) extends Field[C, Int]
  case class LongField[C](fieldName: String, collection: Make[C]) extends Field[C, Long]

  case class InnerField[C, F](fieldName: String, collection: Make[C]) extends Field[C, F]



  object MongoBuilder {

    type selectFields = SelectFields[_]
    type update = Update[_]
    type join = Join[_,_,_]

    def buildInsertResultAsString[C](i: InsertResult[C]) = {
      "db.%s.insert(%s)".format(i.t, i.c.toString)
    }

    def builderSelectResult[F](s: SelectResult[_]):execute = {
      selectExecute(s.c.toString, buildCondition(s.s.w), buildSelectFields(s.s), s.tag)
    }

    def builderUpdateResult[F](s: UpdateResult[_]):execute = {
      updateExecute(s.c.toString, buildCondition(s.s.condition.c), buildModify(s.s))
    }

    def buildSelectResultAsString(s: SelectResult[_]) = {
      val fields = buildSelectFields(s.s)
      val condition = buildCondition(s.s.w).toString
      val collection = s.c
      if (fields.keySet().isEmpty) {
        "db.%s.find(%s)".format(collection, condition)
      } else {
        "db.%s.find(%s, %s)".format(collection, condition, fields)
      }
    }

    def buildUpdateResultAsString(u: UpdateResult[_]) = {
      val collection = u.c
      val condition = buildCondition(u.s.condition.c).toString
      val modify = buildModify(u.s)
      "db.%s.update(%s, %s)".format(collection, condition, modify)
    }

    def buildJoin(joins: List[join]):DBObject = {
      val dbList = new BasicDBList()
      joins.foreach { join =>
        val builder = BasicDBObjectBuilder.start()
        dbList.add(builder.append("$lookup", BasicDBObjectBuilder.start()
          .append("from", join.joined.collection.toString)
          .append("localField", join.owner.toString)
          .append("foreignField", join.joined.toString)
          .append("as", "copies_sold").get()).get())
      }
      dbList
    }

    def buildJoinResultAsString(join: JoinResult[_,_]):String = {
      "db.%s.aggregate(%s)".format(join.c, buildJoin((join.joined.stack :+ join.joined).toList).toString)
    }

    def buildModify(modify: update):DBObject = {
      val builder = BasicDBObjectBuilder.start()
      modify.modify.foreach {
        s => s._1 match {
          case `setExpression` => builder.append("$set", buildListToDBObject(s._2))
          case `minExpression` => builder.append("$min", buildListToDBObject(s._2))
          case `maxExpression` => builder.append("$max", buildListToDBObject(s._2))
          case `renameExpression` => builder.append("$rename", buildListToDBObject(s._2))
          case `incExpression` => builder.append("$inc", buildListToDBObject(s._2))
          case `mulExpression` => builder.append("$mul", buildListToDBObject(s._2))
          case `unsetExpression` => builder.append("$unset", buildListToDBObject(s._2))
        }
      }
      builder.get()
    }

    def buildListToDBObject(list: List[update]) = {
      val builder = BasicDBObjectBuilder.start()
      list.foreach(a => builder.append(a.left.toString, a.right))
      builder.get()
    }

    def buildSelectFields(select: SelectExpression) = {
      val builder = BasicDBObjectBuilder.start()
      select match {
        case e: selectFields => e.c.foreach { a => builder.append(a.toString, 1) }
        case _ =>
      }
      builder.get()
    }

    def buildCondition(predicate: Expression[_], builder: BasicDBObjectBuilder = BasicDBObjectBuilder.start()):DBObject = {
      predicate match {
        case b: BooleanExpression[_,_] =>
          b.operator match {
            case "$eq" => builder.append(b.left.toString, b.getRight).get
            case _ => builder.append(
              b.left.toString, new BasicDBObject(b.operator, b.getRight)).get
          }
        case l: LogicalExpression[_] =>
          builder.append(l.operator, (buildCondition(l.left)::buildCondition(l.right)::Nil).toArray).get
      }
    }

  }

}
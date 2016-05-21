package record

import com.mongodb.{BasicDBList, BasicDBObject, DBObject, BasicDBObjectBuilder}
import DBObjectSerializer._
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

  type se = SelectEntity[_]
  type sf1 = SelectFields1[_,_]
  type sf2 = SelectFields2[_,_, _]
  type sf3 = SelectFields3[_,_, _, _]
  type update = Update[_]
  type join = Join[_,_,_]
  type joinOne = JoinOne[_,_,_]
  type joinMany = JoinMany[_,_,_]
  val se = selectExecute
  val sf = selectFieldsExecute

  trait Make[C]

  abstract class Meta[C: TypeTag] extends Make[C] {
    val collection_name:String
    override def toString:String = collection_name
    def insert(c: C):InsertResult[C] = InsertResult(this, DBObjectSerializer.asDBObjectImplicit(c, typeOf2))
    def isValid(c: C):Boolean = true
    def modify(c1: this.type => Update[_]): UpdateResult[this.type] = UpdateResult(this, c1(this))
    def where(c1: Expression[C]): WhereExpression[C] = WhereExpression(c1)
    //TODO подумать на повышение возвращаемого значения. убрать Option[_]
    def where: WhereExpression[C] = WhereExpression(allExpression(), Some(this))
    def where(c1: this.type => Expression[C]): WhereExpression[C] = WhereExpression(c1(this), Some(this))
    def find[R](c1: this.type => SelectExpression[R]): SelectResult[R] = SelectResult[R](this, c1(this), typeOf2)
    def copy(collection_name: String = this.collection_name):Meta[C] =  new Meta[C] {
      override val collection_name: String = collection_name
    }
    def typeOf2: Type = typeOf[C]
  }

  case class SelectResult[R](c: M, s: SelectExpression[R], tag: Type) extends Query[R] {
    override def toString: String = MongoBuilder.buildSelectResultAsString(this)

    override def execute: execute[R] = MongoBuilder.builderSelectResult(this)
  }

  case class JoinResult[R](joined: JoinQueryYield[R], collection: M) extends Query[R] {
    override def toString: String =  MongoBuilder.buildJoinResultAsString(this)

    override def execute: execute[R] = MongoBuilder.buildJoinResult(this)
  }

  trait JoinQueryYield[R] { val condition: Expression[_]; val joinExpression:Seq[join] }
  case class JoinQueryYield1[C1, C2](condition: Expression[_], j1: Join[C1, C2, _]) extends JoinQueryYield[(C1, C2)] { val joinExpression = Seq(j1) }
  case class JoinQueryYield2[C1, C2, C3](condition: Expression[_], j1: Join[C1, C2, _], j2: Join[_, C3, _]) extends JoinQueryYield[(C1, C2, C3)] { val joinExpression = Seq(j1, j2) }

  trait SelectExpression[R] {
    val w: Expression[_]
  }

  case class SelectEntity[C](w: Expression[_], c: M) extends SelectExpression[C]

  case class SelectFields1[C, F1](w: Expression[_], c1: Field[C, F1]) extends SelectExpression[F1]
  case class SelectFields2[C, F1, F2](w: Expression[_], c1: Field[C, F1], c2: Field[C, F2]) extends SelectExpression[(F1, F2)]
  case class SelectFields3[C, F1, F2, F3](w: Expression[_], c1: Field[C, F1], c2: Field[C, F2], c3: Field[C, F3]) extends SelectExpression[(F1, F2, F3)]

  case class UpdateResult[T <: M](c: T, s: Update[_]) extends Query[Any] {
    override def toString: String = MongoBuilder.buildUpdateResultAsString(this)

    override def execute: execute[Any] = MongoBuilder.builderUpdateResult(this)
  }

  case class WhereExpression[C](c: Expression[C], collection: Option[M] = None) extends Update[C] with Query[Any] {
    val parent = null
    val left = null
    val right = null
    val symbol: String = null

    def select(c1: M) = SelectEntity[C](c, c1)
    def select[F](c1: Field[C, F]) = SelectFields1(c, c1)
    def select[F1, F2](c1: Field[C, F1], c2: Field[C, F2]) = SelectFields2(c, c1, c2)
    def select[F1, F2, F3](c1: Field[C, F1], c2: Field[C, F2], c3: Field[C, F3]) = SelectFields3(c, c1, c2, c3)

    def on[C1, F](f: => Join[C,C1,F]) = JoinQueryYield1(c, f)
    def on[C1, C2, F](f1: => Join[C,C1,F], f2: => Join[_,C2,F]) = JoinQueryYield2(c, f1, f2)

    override def execute: execute[Any] = MongoBuilder.builderWhereExpression(this)
  }

  trait Expression[T] {
    def &&(r: Expression[T]) = LogicalExpression(this, r, "$and")

    def ||(r: Expression[T]) = LogicalExpression(this, r, "$or")
  }

  case class allExpression[C]() extends Expression[C]

  /**
   *
   * @param left
   * @param right
   * @param operator
   * @tparam C collection
   * @tparam F field
   */
  case class BooleanExpression[C, F](left: Field[C, F], right: F, operator: String)(implicit ev1: TypeTag[F]) extends Expression[C] {
    def getRight:Any = DBObjectSerializer.asDBObjectImplicit(right, typeOf[F])
  }

  case class LogicalExpression[C](left: Expression[C], right: Expression[C], operator: String) extends Expression[C]

  trait Join[C,C1,F] {
    val owner: Field[_, F]
    val joined: Field[_, F]
  }


  case class JoinOne[C, C1, F](owner: Field[C, F], joined: Field[C1, F]) extends Join[C,Option[C1],F]
  case class JoinMany[C, C1, F](owner: Field[C, F], joined: Field[C1, F]) extends Join[C,List[C1],F]

  case class InsertResult[C](t: Meta[C], c: Any) extends Query[Any] {
    override def toString: String = MongoBuilder.buildInsertResultAsString(this)

    override def execute: execute[Any] = MongoBuilder.builderInsertResult(this)
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
    //TODO hide private[_]
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

    def ===[C1](joined: Field[C1, F]): Join[C,Option[C1],F] = JoinOne(this, joined)

    def hashOne[C1](joined: Field[C1, F]): Join[C,Option[C1],F] = JoinOne(this, joined)

    def hashMany[C1](joined: Field[C1, F]): Join[C,List[C1],F] = JoinMany(this, joined)

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

    def typeOf2:Type

  }

  case class UField[C, F](fieldName: String, collection: Make[C]) extends Field[C, F] {
    override def typeOf2: Type = ???
  }
  case class StringField[C](fieldName: String, collection: Make[C]) extends Field[C, String] {
    override def typeOf2: Type = typeOf[String]
  }
  case class IntField[C](fieldName: String, collection: Make[C]) extends Field[C, Int] {
    override def typeOf2: Type = typeOf[Int]
  }
  case class LongField[C](fieldName: String, collection: Make[C]) extends Field[C, Long] {
    override def typeOf2: Type = typeOf[Long]
  }

  case class InnerField[C, F](fieldName: String, collection: Make[C])(implicit t: TypeTag[F]) extends Field[C, F] {
    override def typeOf2: Type = typeOf[F]
  }



  object MongoBuilder {

    def buildInsertResultAsString[C](i: InsertResult[C]) = {
      "db.%s.insert(%s)".format(i.t, i.c.toString)
    }

    def builderInsertResult[F](i: InsertResult[_]):execute[Any] = {
      insertExecute(i.t.toString, i.c.asInstanceOf[DBObject])
    }

    def builderWhereExpression(s: WhereExpression[_]):execute[Any] = {
      conditionExecute(s.collection.get.toString, buildCondition(s.c))
    }

    def builderSelectResult[R](s: SelectResult[R]):execute[R] = {

      def as(a:DBObject, e: Field[_,_]) = fromDBObject(a.get(e.fieldName), e.typeOf2)

      s.s match {
        case e: se => se(s.c.toString, buildCondition(s.s.w), asObject[R](_, s.c.typeOf2))
        case e: sf1 => sf(s.c.toString, buildCondition(s.s.w), buildSelectFields(s.s), a => fromDBObject(a.get(e.c1.fieldName), e.c1.typeOf2).asInstanceOf[R])
        case e: sf2 => sf(s.c.toString, buildCondition(s.s.w), buildSelectFields(s.s), a => (as(a, e.c1), as(a, e.c2)).asInstanceOf[R])
        case e: sf3 => sf(s.c.toString, buildCondition(s.s.w), buildSelectFields(s.s), a => (as(a, e.c1), as(a, e.c2), as(a, e.c3)).asInstanceOf[R])
      }
    }

    def builderUpdateResult[F](s: UpdateResult[_]):execute[Any] = {
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

    def buildJoin(joins: Seq[join]):Seq[DBObject] = {
      joins.map { join =>
        val builder = BasicDBObjectBuilder.start()
        builder.append("$lookup", BasicDBObjectBuilder.start()
          .append("from", join.joined.collection.toString)
          .append("localField", join.owner.toString)
          .append("foreignField", join.joined.toString)
          .append("as", join.joined.collection.toString).get()).get()
      }
    }

    def buildJoinResultAsString(join: JoinResult[_]):String = {
      val dblist = new BasicDBList()
      buildJoin(join.joined.joinExpression).foreach {dblist.add}
      "db.%s.aggregate(%s)".format(join.joined, dblist.toString)
    }

    def buildJoinResult[R](join: JoinResult[R]):execute[R] = {

      def relation(j: join, dbo: DBObject) = {
        val collection = dbo.get(j.joined.collection.toString).asInstanceOf[BasicDBList].toArray.toList
        j match {
          case y: joinOne =>
            collection.headOption.map(fromDBObject(_, y.joined.collection.asInstanceOf[M].typeOf2))
          case y: joinMany =>
            collection.map(fromDBObject(_, y.joined.collection.asInstanceOf[M].typeOf2))
        }
      }

      val condition =  new BasicDBObject("$match", buildCondition(join.joined.condition))
      val joins = buildJoin(join.joined.joinExpression)//TODO

      joinExecute[R](join.collection.toString, condition::joins.toList, { m =>

        val head = fromDBObject(m, join.collection.typeOf2)

        join.joined match {
          case JoinQueryYield1(c,j1) => (head, relation(j1, m)).asInstanceOf[R]
          case JoinQueryYield2(c,j1, j2) => (head, relation(j1, m), relation(j2, m)).asInstanceOf[R]
        }

      })
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

    def buildSelectFields[R](select: SelectExpression[R]) = {
      val builder = BasicDBObjectBuilder.start()
      select match {
        case e: sf1 => builder.append(e.c1.toString, 1)
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
        case allExpression() => new BasicDBObject()
      }
    }

  }

}
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

  case class JoinResult[T <: M, T1 <: M](c: T, c1: T1, joined: Join[_, _, _]) extends Query[Any] {
    override def toString: String = MongoBuilder.buildJoinResultAsString(this)

    override def execute: execute[Any] = MongoBuilder.buildJoinResult(this)
  }

  trait SelectExpression[R] {
    val w: Expression[_]
  }

  case class SelectEntity[C](w: Expression[_], c: M) extends SelectExpression[C]

  case class SelectFields[C, F](w: Expression[_], c: Field[C, F]) extends SelectExpression[F]
  case class SelectFields2[C, F1, F2](w: Expression[_], c: Field[C, F1], c1: Field[C, F2]) extends SelectExpression[(F1, F2)]

  case class UpdateResult[T <: M](c: T, s: Update[_]) extends Query[Any] {
    override def toString: String = MongoBuilder.buildUpdateResultAsString(this)

    override def execute: execute[Any] = MongoBuilder.builderUpdateResult(this)
  }

  case class WhereExpression[C](c: Expression[C], collection: Option[M] = None) extends Update[C] with Query[Any] {
    val parent = null
    val left = null
    val right = null
    val symbol: String = null
    //TODO подумать над удалением
    def select(c1: M) = SelectEntity[C](c, c1)
    def select[F](c1: Field[C, F]) = SelectFields(c, c1)
    def select[F1, F2](c1: Field[C, F1], c2: Field[C, F2]) = SelectFields2(c, c1, c2)
    def on[C1, F](f:WhereExpression[_] => Join[C,C1,F]): Join[C, C1, F] = f(this)
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
    val owner: Field[C, F]
    val joined: Field[C1, F]
    val w: WhereExpression[_]
    val parent: Option[Join[_,_,_]]
    def flatten:List[Join[_,_,_]] = parent match {
      case Some(x) => this::flatten
      case None => this::Nil
    }
    def on[C2](f: WhereExpression[_] => Join[C,C2,F]) = f(w) match {
      case x: JoinOne[_,_,_] => x.copy(parent = Some(this))
      case x: JoinMany[_,_,_] => x.copy(parent = Some(this))
    }
  }


  case class JoinOne[C, C1, F](owner: Field[C, F], joined: Field[C1, F], w: WhereExpression[_], parent: Option[Join[_,_,_]] = None) extends Join[C,C1,F]
  case class JoinMany[C, C1, F](owner: Field[C, F], joined: Field[C1, F], w: WhereExpression[_], parent: Option[Join[_,_,_]] = None) extends Join[C,C1,F]

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

    def ===[C1](joined: Field[C1, F]): WhereExpression[_] => Join[C,C1,F] = { w => JoinOne(this, joined, w) }

    def hashOne[C1](joined: Field[C1, F]): WhereExpression[_] => Join[C,C1,F] = { w => JoinOne(this, joined, w) }

    def hashMany[C1](joined: Field[C1, F]): WhereExpression[_] => Join[C,C1,F] = { w => JoinMany(this, joined, w) }

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

    type selectFields = SelectFields[_,_]
    type selectFields2 = SelectFields2[_,_, _]
    type selectEntity = SelectEntity[_]
    type update = Update[_]
    type join = Join[_,_,_]
    type joinOne = JoinOne[_,_,_]
    type joinMany = JoinMany[_,_,_]

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
      s.s match {
        case e: selectFields => selectFieldsExecute(s.c.toString, buildCondition(s.s.w), buildSelectFields(s.s), { obj =>
            DBObjectSerializer.fromDBObjectType(obj.get(e.c.fieldName), e.c.typeOf2).asInstanceOf[R]
        })
        case e: selectEntity => selectExecute(s.c.toString, buildCondition(s.s.w), { obj =>
          DBObjectSerializer.fromDBObjectType(obj, s.c.typeOf2).asInstanceOf[R]
        })
        case e: selectFields2 => selectFieldsExecute(s.c.toString, buildCondition(s.s.w), buildSelectFields(s.s), { obj =>
          (DBObjectSerializer.fromDBObjectType(obj.get(e.c.fieldName), e.c.typeOf2),
          DBObjectSerializer.fromDBObjectType(obj.get(e.c1.fieldName), e.c1.typeOf2)).asInstanceOf[R]
        })
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

    def buildJoin(joins: List[join]):List[DBObject] = {
      joins.map { join =>
        val builder = BasicDBObjectBuilder.start()
        builder.append("$lookup", BasicDBObjectBuilder.start()
          .append("from", join.joined.collection.toString)
          .append("localField", join.owner.toString)
          .append("foreignField", join.joined.toString)
          .append("as", join.joined.collection.toString).get()).get()
      }
    }

    def buildJoinResultAsString(join: JoinResult[_,_]):String = {
      val dblist = new BasicDBList()
      buildJoin(join.joined.flatten).foreach {dblist.add}
      "db.%s.aggregate(%s)".format(join.c, dblist.toString)
    }

    def buildJoinResult[T <: M](join: JoinResult[T,_]):execute[Any] = {

      import DBObjectSerializer.{fromDBObjectType => fromDBObject}

      val condition =  new BasicDBObject("$match", buildCondition(join.joined.w.c))
      val joins = buildJoin(join.joined.flatten)
      joinExecute(join.c.toString, condition::joins, { m =>
        val head = fromDBObject(m, join.c.typeOf2)
        val tail = join.joined.flatten.map { join =>
          val joinCollection = m.get(join.joined.collection.toString).asInstanceOf[BasicDBList].toArray.toList
          join match {
            case y: joinOne =>
              joinCollection.headOption.map(fromDBObject(_, y.joined.collection.asInstanceOf[M].typeOf2))
            case y: joinMany =>
              joinCollection.map(fromDBObject(_, y.joined.collection.asInstanceOf[M].typeOf2))
          }
        }
        head::tail
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
        case e: selectFields => builder.append(e.c.toString, 1)
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
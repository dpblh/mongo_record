package record

/**
 * Created by tim on 29.04.16.
 */
trait Lexis {

  def classAsString[C](c: C):String

  type M = Meta[_]

  trait Make[C]

  trait Meta[C] extends Make[C] {
    val collection_name:String
    override def toString:String = collection_name
    def insert(c: C):InsertResult[C] = InsertResult(this, c, classAsString)
    def isValid(c: C):Boolean = true
    def apply(c1: this.type => SelectExpression): SelectResult[this.type] = SelectResult(this, c1(this))
    def copy(collection_name: String = this.collection_name):Meta[C] =  new Meta[C] {
      override val collection_name: String = collection_name
    }
  }

  case class MapReduceResult[T <: M](c: T, s: Reduce[_]) extends Query {
    override def toString: String = {
      "db.%s.mapReduce(%s)".format(c, s)
    }
  }

  case class SelectResult[T <: M](c: T, s: SelectExpression) extends Query {
    override def toString: String = {
      "db.%s.find(%s)".format(c, s)
    }
  }

  case class JoinResult[T <: M, T1 <: M](c: T, c1: T1, joined: Join[_, _, _]) extends Query {
    override def toString: String = {
      "db.%s.aggregate([%s])".format(c, joined)
    }
  }

  trait SelectExpression extends Query

  case class SelectEntity[T <: M](w: Expression[_], c: T) extends SelectExpression {
    override def toString: String = {
      "{%s}".format(w)
    }
  }

  case class SelectFields[C](w: Expression[_], c: Seq[Field[C, _]]) extends SelectExpression {
    override def toString: String = {
      "{%s}, {%s}".format(w, c.map(f => s"$f : 1").mkString(","))
    }
  }

  case class UpdateResult[T <: M](c: T, s: Update[_]) extends Query {
    override def toString: String = {
      val where = s.flatten.get("WhereExpression").get.head
      val update = s.flatten.filter(_._1 != "WhereExpression")
      val r = update.map {
        s => s._1 match {
          case "SetExpression" => SetExpression.toString(s._2)
          case "MinExpression" => MinExpression.toString(s._2)
          case "MaxExpression" => MaxExpression.toString(s._2)
          case "RenameExpression" => RenameExpression.toString(s._2)
          case "IncExpression" => IncExpression.toString(s._2)
          case "MulExpression" => MulExpression.toString(s._2)
          case "UnsetExpression" => UnsetExpression.toString(s._2)
        }
      }
      "db.%s.update(%s, {%s})".format(c, where, r.mkString(", "))
    }
  }

  case class WhereExpression[C](c: Expression[C]) extends Query with Update[C] {
    val parent = null
    val left = null
    val right = null
    def select[S <: Meta[C]](c1: S) = {
      SelectEntity(c, c1)
    }

    def select(c1: Field[C, _]*) = {
      SelectFields(c, c1)
    }

    def on[C1, F](f: => Join[C, C1, F]): Join[C, C1, F] = f

    def emit[K, V](key: Field[C, K], value: Field[C, V]) = Emit(key, value)

    override def toString: String = {
      s"{${c.toString}}"
    }
  }

  case class Emit[C, K, V](key: Field[C, K], value: Field[C, V]) {
    def sum = Sum(this, value)

    def max = Max(this, value)

    override def toString: String = {
      "emit(e.%s, e.%s)".format(key, value)
    }
  }

  trait Reduce[C]

  case class Max[C, V](e: Emit[C, _, V], value: Field[C, V]) extends Reduce[C] {
    override def toString: String = {
      "function(e){%s}, function(key, values){Array.max(values)}".format(e)
    }
  }

  case class Sum[C, V](e: Emit[C, _, V], value: Field[C, V]) extends Reduce[C] {
    override def toString: String = {
      "function(e){%s}, function(key, values){Array.sum(values)}".format(e)
    }
  }

  trait Query

  trait Expression[T] extends Query {
    def &&(r: Expression[T]) = LogicalExpression(this, r, "and")

    def ||(r: Expression[T]) = LogicalExpression(this, r, "or")
  }

  /**
   *
   * @param left
   * @param right
   * @param operator
   * @tparam C collection
   * @tparam F field
   */
  case class BooleanExpression[C, F](left: Field[C, F], right: F, operator: String) extends Expression[C] {
    override def toString: String = {
      operator match {
        case "eq" => s"${left.toString} : '${right.toString}'"
        case "gt" => "%s : { $gt : '%s' }".format(left.toString, right.toString)
        case "lt" => "%s : { $lt : '%s' }".format(left.toString, right.toString)
        case _ => "noop"
      }
    }
  }

  case class LogicalExpression[C](left: Expression[C], right: Expression[C], operator: String) extends Expression[C] {
    override def toString: String = {
      operator match {
        case "and" => "$and : [{%s}, {%s}]".format(left.toString, right.toString)
        case "or" => "$or : [{%s}, {%s}]".format(left.toString, right.toString)
      }
    }
  }

  case class Join[C, C1, F](owner: Field[C, F], joined: Field[C1, F], stack: Seq[Join[_, _, _]]) extends Query {
    def on[C2](f: => Join[C, C2, F]) = this.copy(stack = stack :+ f)

    override def toString: String = {
      (stack :+ this).map { join =>
        """{
          |  $lookup: {
          |    from: "%s",
          |    localField: "%s",
          |    foreignField: "%s",
          |    as: "copies_sold"
          |  }
          |}
        """.stripMargin.format(join.joined.collection, join.owner, join.joined)
      }.mkString(", ")
    }
  }

  case class InsertResult[C](t: Meta[C], c: C, f: C => String) extends Query {
    override def toString: String = "db.%s.insert(%s)".format(t.collection_name, f(c))
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
    def flatten:Map[String, List[Update[C]]] = {

      val parents = scala.collection.mutable.ArrayBuffer[Update[C]](this)
      var p = this

      while (p.parent != null) {
        parents += p.parent
        p = p.parent
      }
      parents.toList.groupBy(_.getClass.getSimpleName)
    }
  }

  case class SetExpression[C, F](parent: Update[C], left: Field[C, F], right: F) extends Update[C]
  object SetExpression {
    def toString(list: List[Update[_]]):String = {
      "$set: {" +list.reverse.mkString(", ") + "}"
    }
  }

  case class IncExpression[C, F](parent: Update[C], left: Field[C, F], right: F) extends Update[C]

  object IncExpression {
    def toString(list: List[Update[_]]):String = {
      "$inc: {" +list.reverse.mkString(", ") + "}"
    }
  }

  case class MulExpression[C, F](parent: Update[C], left: Field[C, F], right: F) extends Update[C]

  object MulExpression {
    def toString(list: List[Update[_]]):String = {
      "$mul: {" +list.reverse.mkString(", ") + "}"
    }
  }

  case class RenameExpression[C, F](parent: Update[C], left: Field[C, F], right: String) extends Update[C]

  object RenameExpression {
    def toString(list: List[Update[_]]):String = {
      "$rename: {" +list.reverse.mkString(", ") + "}"
    }
  }

  case class UnsetExpression[C, F](parent: Update[C], left: Field[C, F]) extends Update[C] {
    override val right = ""
  }

  object UnsetExpression {
    def toString(list: List[Update[_]]):String = {
      "$unset: {" +list.reverse.mkString(", ") + "}"
    }
  }

  case class MinExpression[C](parent: Update[C], left: Field[C, Int], right: Int) extends Update[C]

  object MinExpression {
    def toString(list: List[Update[_]]):String = {
      "$min: {" +list.reverse.mkString(", ") + "}"
    }
  }

  case class MaxExpression[C](parent: Update[C], left: Field[C, Int], right: Int) extends Update[C]

  object MaxExpression {
    def toString(list: List[Update[_]]):String = {
      "$max: {" +list.reverse.mkString(", ") + "}"
    }
  }

  /**
   *
   * @tparam C collection
   * @tparam F field
   */
  // implicit where
  trait Field[C, F] extends Make[C] {

    val fieldName: String
    val collection: Make[C]

    def ===(right: F) = BooleanExpression(this, right, "eq")

    def ===[C1](joined: Field[C1, F]) = Join(this, joined, Seq())

    def >(right: F) = BooleanExpression(this, right, "gt")

    def <(right: F) = BooleanExpression(this, right, "lt")

    def >=(right: F) = BooleanExpression(this, right, "gte")

    def <=(right: F) = BooleanExpression(this, right, "lte")

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

}
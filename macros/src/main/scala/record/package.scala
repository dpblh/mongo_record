/**
 * Created by tim on 25.04.16.
 */
package object record {

  type M = MongoRecord#Make[_]

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

  case class UpdateResult[T <: M](c: T, s: UpdateExpression[T]) extends Query {
    override def toString: String = {
      "db.%s.update(%s)".format(c, s)
    }
  }

  case class UpdateExpression[T <: M](w: Expression[_], c: Seq[SetExpression[_, _]]) extends Query {
    override def toString: String = {
      """{%s}, { $set : {%s} }""".format(w, c.mkString(", "))
    }
  }


  case class WhereExpression[C](c: Expression[C]) extends Query {
    def select[S <: MongoRecord#Make[C]](c1: S) = {
      SelectEntity(c, c1)
    }

    def select(c1: Field[C, _]*) = {
      SelectFields(c, c1)
    }

    def set[S <: MongoRecord#Make[C]](update: SetExpression[C, _]*): UpdateExpression[S] = {
      UpdateExpression[S](c, update)
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

  case class SetExpression[C, F](left: Field[C, F], right: F) extends Query {
    override def toString: String = {
      s"${left.toString} : '${right.toString}'"
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

  case class InsertResult[C](t: MongoRecord#Make[C], c: C, f: C => String) extends Query {
    override def toString: String = "db.%s.insert(%s)".format(t.collection_name, f(c))
  }

  /**
   *
   * @tparam C collection
   * @tparam F field
   */
  // implicit where
  trait Field[C, F] extends WO[C] {

    val fieldName: String
    val collection: WO[C]

    def ===(right: F) = BooleanExpression(this, right, "eq")

    def ===[C1](joined: Field[C1, F]) = Join(this, joined, Seq())

    def >(right: F) = BooleanExpression(this, right, "gt")

    def <(right: F) = BooleanExpression(this, right, "lt")

    def >=(right: F) = BooleanExpression(this, right, "gte")

    def <=(right: F) = BooleanExpression(this, right, "lte")

    def :=(right: F) = SetExpression(this, right)

    override def toString: String = {
      collection match {
        case x: MongoRecord#Make[C] => fieldName
        case x: Field[C, _] => x+"."+fieldName
      }
    }

  }

  case class UField[C, F](fieldName: String, collection: WO[C]) extends Field[C, F]
  case class StringField[C](fieldName: String, collection: WO[C]) extends Field[C, String]
  case class IntField[C](fieldName: String, collection: WO[C]) extends Field[C, Int]
  case class LongField[C](fieldName: String, collection: WO[C]) extends Field[C, Long]

}

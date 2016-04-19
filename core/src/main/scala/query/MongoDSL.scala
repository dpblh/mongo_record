package query

import org.json4s.{Formats, DefaultFormats}

/**
 * Created by tim on 18.04.16.
 */
trait MongoDSL {

  /**
   * просто карринг. для установки базового типа
   * @param c
   * @param c1
   * @tparam T Collection type
   * @return
   */
  //можно попробовать через case class
  protected def from[T <: Make[_]](c: T)(c1: T => SelectExpression[T, _]): SelectExpression[T, _] = c1(c)

  //  def from[T,C](c: T)(c1: T => SelectExpression[T, C])(implicit conv: T => Make[C]):SelectExpression[T, C] = c1(c)

  def update[T <: Make[_]](c: T)(c1: T => UpdateExpression[T, _]): UpdateExpression[T, _] = c1(c)

  /**
   * predicate builder
   * @param c
   * @tparam C Collection
   * @return
   */
  protected def where[C <: AnyRef](c: Expression[C]): WhereExpression[C] = WhereExpression(c)


  trait Make[C <: AnyRef] {
    implicit def json4sJacksonFormats: Formats = DefaultFormats
    import org.json4s.jackson.Serialization.write
    val collection_name:String
    override def toString:String = collection_name
    def insert(c: C):String = {
      "db.%s.insert(%s)".format(collection_name, write(c))
    }
  }

  case class SelectExpression[S <: Make[C], C <: AnyRef](w: Expression[C], c: S) extends Query {
    override def toString: String = {
      s"db.${c.toString}.find({$w})"
    }
  }

  case class UpdateExpression[T <: Make[C], C <: AnyRef](w: Expression[C], c: Seq[SetExpression[C, _]]) extends Query {
    override def toString: String = {
      "db.TODO.update({%s}, {%s})".format(w, c.mkString(", "))
    }
  }


  case class WhereExpression[C <: AnyRef](c: Expression[C]) extends Query {
    def select[S <: Make[C]](c1: S) = {
      SelectExpression(c, c1)
    }
    def set[S <: Make[C]](update: SetExpression[C, _]*):UpdateExpression[S, C] = {
      UpdateExpression[S, C](c, update)
    }
    override def toString:String = {
      s"{${c.toString}}"
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
    override def toString:String = {
      operator match {
        case "eq" => s"${left.toString} : '${right.toString}'"
        case "gt" => "%s : { $gt : '%s' }".format(left.toString, right.toString)
        case "lt" => "%s : { $lt : '%s' }".format(left.toString, right.toString)
        case _ => "noop"
      }
    }
  }

  case class LogicalExpression[C](left: Expression[C], right: Expression[C], operator: String) extends Expression[C] {
    override def toString:String = {
      operator match {
        case "and" => "$and : [{%s}, {%s}]".format(left.toString, right.toString)
        case "or" => "$or : [{%s}, {%s}]".format(left.toString, right.toString)
      }
    }
  }

  case class SetExpression[C, F](left: Field[C,F], right: F) extends Query {
    override def toString:String = {
      s"${left.toString} : '${right.toString}'"
    }
  }

  /**
   *
   * @param fieldName
   * @tparam C collection
   * @tparam F field
   */
  case class Field[C, F](fieldName: String) {
    def ===(right: F) = BooleanExpression(this, right, "eq")

    def >(right: F) = BooleanExpression(this, right, "gt")

    def <(right: F) = BooleanExpression(this, right, "lt")

    def >=(right: F) = BooleanExpression(this, right, "gte")

    def <=(right: F) = BooleanExpression(this, right, "lte")

    def := (right: F) = SetExpression(this, right)

    override def toString:String = fieldName

  }


}

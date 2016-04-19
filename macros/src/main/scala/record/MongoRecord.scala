package record

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
 * Created by tim on 18.04.16.
 */
trait MongoRecord {

  def classAsString[C <: AnyRef](c: C):String

  /**
   * просто карринг. для установки базового типа
   * @param c
   * @param c1
   * @tparam T Collection type
   * @return
   */
  def from[T <: Make[_]](c: T)(c1: T => SelectExpression[T]): SelectResult[T] = SelectResult[T](c, c1(c))

  def update[T <: Make[_]](c: T)(c1: T => UpdateExpression[T]): UpdateResult[T] = UpdateResult(c, c1(c))

  /**
   * predicate builder
   * @param c
   * @tparam C Collection
   * @return
   */
  def where[C <: AnyRef](c: Expression[C]): WhereExpression[C] = WhereExpression(c)


  trait Make[C <: AnyRef] {
    val collection_name:String
    override def toString:String = collection_name
    def insert(c: C):String = {
      "db.%s.insert(%s)".format(collection_name, classAsString(c))
    }
  }

  case class SelectResult[T  <: Make[_]](c: T, s: SelectExpression[T]) extends Query {
    override def toString: String = {
      "db.%s.find(%s)".format(c, s)
    }
  }

  case class SelectExpression[S <: Make[_]](w: Expression[_], c: S) extends Query {
    override def toString: String = {
      "{%s}".format(w)
    }
  }

  case class UpdateResult[T  <: Make[_]](c: T, s: UpdateExpression[T]) extends Query {
    override def toString: String = {
      "db.%s.update(%s)".format(c, s)
    }
  }

  case class UpdateExpression[T <: Make[_]](w: Expression[_], c: Seq[SetExpression[_, _]]) extends Query {
    override def toString: String = {
      """{%s}, { $set : {%s} }""".format(w, c.mkString(", "))
    }
  }


  case class WhereExpression[C <: AnyRef](c: Expression[C]) extends Query {
    def select[S <: Make[C]](c1: S) = {
      SelectExpression(c, c1)
    }
    def set[S <: Make[C]](update: SetExpression[C, _]*):UpdateExpression[S] = {
      UpdateExpression[S](c, update)
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

object MongoRecord extends UtilsMacro {

  def meta[T]: Any = macro metaImpl[T]

  def metaImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val tpe = weakTypeOf[T]

    var fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (name, typ) = p
      q"val ${TermName(name.encoded)} = Field[$tpe, $typ](${name.encoded})"
    }.toList

    val collection_name = "person"

    fields = q"val collection_name = $collection_name" ::fields

    q"new Make[$tpe] { ..$fields }"

  }

}
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
  def from[T <: Make[_]](c: T)(c1: T => SelectExpression): SelectResult[T] = SelectResult[T](c, c1(c))

  def update[T <: Make[_]](c: T)(c1: T => UpdateExpression[T]): UpdateResult[T] = UpdateResult(c, c1(c))

  def mapReduce[T <: Make[_]](c: T)(c1: T => Reduce[_]): MapReduceResult[T] = MapReduceResult(c, c1(c))

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

  case class MapReduceResult[T  <: Make[_]](c: T, s: Reduce[_]) extends Query {
    override def toString: String = {
      "db.%s.mapReduce(%s)".format(c, s)
    }
  }

  case class SelectResult[T  <: Make[_]](c: T, s: SelectExpression) extends Query {
    override def toString: String = {
      "db.%s.find(%s)".format(c, s)
    }
  }

  trait SelectExpression extends Query

  case class SelectEntity[T <: Make[_]](w: Expression[_], c: T) extends SelectExpression {
    override def toString: String = {
      "{%s}".format(w)
    }
  }

  case class SelectFields[C](w: Expression[_], c: Seq[Field[C, _]]) extends SelectExpression {
    override def toString: String = {
      "{%s}, {%s}".format(w, c.map(f => s"$f : 1").mkString(","))
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
      SelectEntity(c, c1)
    }
    def select(c1: Field[C, _]*) = {
      SelectFields(c, c1)
    }
    def set[S <: Make[C]](update: SetExpression[C, _]*):UpdateExpression[S] = {
      UpdateExpression[S](c, update)
    }
    def emit[K, V](key: Field[C, K], value: Field[C, V]) = Emit(key, value)
    override def toString:String = {
      s"{${c.toString}}"
    }
  }

  case class Emit[C, K, V](key: Field[C, K], value: Field[C, V]){
    def sum = Sum(this, value)
    def max = Max(this, value)
    override def toString:String = {
      "emit(e.%s, e.%s)".format(key, value)
    }
  }

  trait Reduce[C]

  case class Max[C, V](e: Emit[C, _, V], value: Field[C, V]) extends Reduce[C] {
    override def toString:String = {
      "function(e){%s}, function(key, values){Array.max(values)}".format(e)
    }
  }

  case class Sum[C, V](e: Emit[C, _, V], value: Field[C, V]) extends Reduce[C] {
    override def toString:String = {
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
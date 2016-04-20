package record

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
 * Created by tim on 18.04.16.
 */
trait MongoRecord {

  type M = Make[_]

  def classAsString[C <: AnyRef](c: C):String

  /**
   * просто карринг. для установки базового типа
   * @param c
   * @param c1
   * @tparam T Collection type
   * @return
   */
  def from[T <: M](c: T)(c1: T => SelectExpression): SelectResult[T] = SelectResult[T](c, c1(c))

  def join[T <: M, T1 <: M](c: T, c1: T1)(f: (T, T1) => Join[_, _, _]): JoinResult[T, T1] = JoinResult[T, T1](c, c1, f(c, c1))

  def join[T <: M, T1 <: M, T2 <: M](c: T, c1: T1, c2: T2)(f: (T, T1, T2) => Join[_, _, _]): JoinResult[T, T1] = JoinResult[T, T1](c, c1, f(c, c1, c2))

  def update[T <: M](c: T)(c1: T => UpdateExpression[T]): UpdateResult[T] = UpdateResult(c, c1(c))

  def mapReduce[T <: M](c: T)(c1: T => Reduce[_]): MapReduceResult[T] = MapReduceResult(c, c1(c))

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

  case class MapReduceResult[T  <: M](c: T, s: Reduce[_]) extends Query {
    override def toString: String = {
      "db.%s.mapReduce(%s)".format(c, s)
    }
  }

  case class SelectResult[T  <: M](c: T, s: SelectExpression) extends Query {
    override def toString: String = {
      "db.%s.find(%s)".format(c, s)
    }
  }

  case class JoinResult[T  <: M, T1  <: M](c: T, c1: T1, joined: Join[_, _, _]) extends Query {
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

  case class UpdateResult[T  <: M](c: T, s: UpdateExpression[T]) extends Query {
    override def toString: String = {
      "db.%s.update(%s)".format(c, s)
    }
  }

  case class UpdateExpression[T <: M](w: Expression[_], c: Seq[SetExpression[_, _]]) extends Query {
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
    def on[C1, F](f: => Join[C, C1, F]):Join[C, C1, F] = f
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

  case class Join[C, C1, F](owner: Field[C, F], joined: Field[C1, F], stack: Seq[Join[_, _, _]]) extends Query {
    def on[C2](f: => Join[C, C2, F]) = this.copy(stack = stack :+ f)
    override def toString:String = {
      (stack :+ this).map { join =>
        """{
          |  $lookup: {
          |    from: "%s",
          |    localField: "%s",
          |    foreignField: "%s",
          |    as: "copies_sold"
          |  }
          |}
        """.stripMargin.format(join.joined.collectionName, join.owner, join.joined)
      }.mkString(", ")
    }
  }

  /**
   *
   * @param fieldName
   * @tparam C collection
   * @tparam F field
   */
  case class Field[C, F](fieldName: String, collectionName: String) {
    def ===(right: F) = BooleanExpression(this, right, "eq")

    def ===[C1](joined: Field[C1, F]) = Join(this, joined, Seq())

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

    val collection_name = tpe.typeSymbol.name.toString.toLowerCase

    var fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (name, typ) = p
      q"val ${TermName(name.encoded)} = Field[$tpe, $typ](${name.encoded}, $collection_name)"
    }.toList

    fields = q"val collection_name = $collection_name" ::fields

    q"new Make[$tpe] { ..$fields }"

  }

}
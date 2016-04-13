package query

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
 * Created by tim on 08.04.16.
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
  protected def from[T <: Make[_]](c: T)(c1: T => Result[T]):Result[T] = c1(c)
//  def from[T,C](c: T)(c1: T => Result[T])(implicit conv: T => Make[C]):Result[T] = c1(c)

  /**
   * predicate builder
   * @param c
   * @tparam F field type
   * @return
   */
  protected def where[F](c: Expression[F]):Predicate[F] = Predicate(c)

}
object MongoDSL extends UtilsMacro {

  def meta[T]: Any = macro metaImpl[T]

  def metaImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val tpe = weakTypeOf[T]

    val fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (name, typ) = p
      q"val ${TermName(name.encoded)} = Field[$tpe, $typ](${name.encoded})"
    }

    q"new Make[$tpe] { ..$fields }"

  }
}

trait Make[C]

case class Result[C](c: C)

case class Predicate[F](c: Expression[F]) {
  def select[T <: Make[_]](c: T): Result[T] = {
    println("select")
    Result(c)
  }
  def select(): Result[F] = ???
}

case class Field[P, C](fieldName: String) {
  def ===(right: C) = BooleanExpression(this, right, "eq")
  def >(right: C) = BooleanExpression(this, right, "gt")
  def <(right: C) = BooleanExpression(this, right, "lt")
  def >=(right: C) = BooleanExpression(this, right, "gte")
  def <=(right: C) = BooleanExpression(this, right, "lte")
}


trait Expression[T] {
  def and(r: Expression[T]) = LogicalExpression(this, r, "and")
  def or(r: Expression[T]) = LogicalExpression(this, r, "or")
}
case class BooleanExpression[P, C](left: Field[P, C], right: C, operator: String) extends Expression[P]
case class LogicalExpression[P, C, C2](left: Expression[P], right: Expression[P], operator: String) extends Expression[P]
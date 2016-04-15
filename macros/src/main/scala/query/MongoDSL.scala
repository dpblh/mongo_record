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
  protected def from[T <: Make[_]](c: T)(c1: T => SelectExpression[T, _]):SelectExpression[T, _] = c1(c)
//  def from[T,C](c: T)(c1: T => SelectExpression[T, C])(implicit conv: T => Make[C]):SelectExpression[T, C] = c1(c)

  /**
   * predicate builder
   * @param c
   * @tparam C Collection
   * @return
   */
  protected def where[C](c: Expression[C]):WhereExpression[C] = WhereExpression(c)


  trait Make[C]

  case class SelectExpression[S <: Make[C], C](w: Expression[C], c: S) extends Query

  case class WhereExpression[C](c: Expression[C]) extends Query {
    def select[S <: Make[C]](c1: S) = {
      SelectExpression(c, c1)
    }
  }

  trait Query
  trait Expression[T] extends Query {
    def and(r: Expression[T]) = LogicalExpression(this, r, "and")
    def or(r: Expression[T]) = LogicalExpression(this, r, "or")
  }

  /**
   *
   * @param left
   * @param right
   * @param operator
   * @tparam C collection
   * @tparam F field
   */
  case class BooleanExpression[C, F](left: Field[C, F], right: F, operator: String) extends Expression[C]
  case class LogicalExpression[C](left: Expression[C], right: Expression[C], operator: String) extends Expression[C]

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
  }


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

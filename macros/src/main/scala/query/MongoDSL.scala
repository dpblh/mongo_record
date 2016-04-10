package query

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
 * Created by tim on 08.04.16.
 */
trait MongoDSL[T] {

  def where(finder: T => Expression) = ???

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

    q"new { ..$fields }"

  }
}


case class Field[P, C](fieldName: String) {
  def gt(right: C) = { finder:P => BooleanExpression(this, right) }
}

trait Expression
case class BooleanExpression[P, C](left: Field[P, C], right: C) extends Expression {
  def and () = ???
}
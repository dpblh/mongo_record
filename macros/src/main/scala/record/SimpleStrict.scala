package record

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
 * Created by tim on 08.04.16.
 */
object SimpleStrict extends UtilsMacro {

  def strict: Any = macro strictImpl
  def strictType[T]: Any = macro strictTypeImpl[T]

  def strictImpl(c: Context) = {
    import c.universe._
    q"new { val x = 2 }"
  }

  def strictTypeImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val tpe = weakTypeOf[T]

    val TypeRef(_, t, _) = tpe

//    println(showRaw(c))
//    throw new Error()

    val fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (name, typ) = p
      q"val ${TermName(name.encoded)} = Field[$tpe, $typ](${name.encoded})"
    }

    q"new { ..$fields }"

//    q"object ${TermName(t.companion.toString)} { val obj = new { ..$fields}}"


  }

}

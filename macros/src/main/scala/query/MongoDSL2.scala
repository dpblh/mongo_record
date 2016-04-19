package query

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
 * Created by tim on 08.04.16.
 */
//TODO временно пока json4s не перенесу
object MongoDSL2 extends UtilsMacro {

  def meta[T]: Any = macro metaImpl[T]

  def metaImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val tpe = weakTypeOf[T]

    var fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (name, typ) = p
      q"val ${TermName(name.encoded)} = Field[$tpe, $typ](${name.encoded})"
    }.toList

    val collection_name = "person"

    fields = q"""val collection_name = $collection_name """::fields

    q"new Make[$tpe] { ..$fields }"

  }

}

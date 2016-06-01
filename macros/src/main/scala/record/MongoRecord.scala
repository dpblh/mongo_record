package record

import record.signatures._

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
 * Created by tim on 18.04.16.
 */
trait MongoRecord
  extends BaseFields
  with FromSignatures
  with JoinSignatures
  with ModifySignatures
  with WhereSignatures
  with MacroWrapper {

  type Meta[C] = ObjectMetaTag[C]

  def meta[T]: MetaTag[T] = macro MongoRecord.metaImpl[T]

}

object MongoRecord extends UtilsMacro {

  def metaImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val tpe = weakTypeOf[T]

    val collection_name = camelToUnderscores(tpe.typeSymbol.name.toString)

    val fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (name, typ) = p
      buildFieldObject(c)(tpe, name, typ)
    }.toList

    q"""new Meta[$tpe] {
        val collection_name = $collection_name
       ..$fields
       }"""

  }

}
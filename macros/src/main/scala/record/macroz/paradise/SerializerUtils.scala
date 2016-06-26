package record.macroz.paradise

import record.ReflectionRecord._

import scala.reflect.macros.whitebox.Context

/**
 * Created by tim on 12.06.16.
 */
object SerializerUtils {

  def getFieldNamesAndTypes(c: Context)(classDef: c.universe.Tree): List[(c.universe.TermName, c.universe.Tree)] = {
    import c.universe._
    classDef match {
      case q"case class $className(..$fields) extends ..$bases { ..$body }" =>
        fields.map { f =>
          (f.name, f.tpt)
        }
      case _ => Nil
    }

  }

  def fieldGenerator(c: Context)(parentTpe: c.universe.ClassDef, name: c.universe.Name, tpe: c.universe.Tree): c.Tree = {
    import c.universe._

    val fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (name, typ) = p
      fieldGenerator(c)(parentTpe, name, typ)
    }

    q"""object ${TermName(name.encoded)} extends record.MacroField[${parentTpe.name}, $tpe](this) {
          import record.macroz.serializer.DBObjectSerializer.{as => asDBO, from => fromDBO}

          private val as = asDBO[$tpe]
          private val from = fromDBO[$tpe]

          override def asDBObject(c: Any):Any = as(c.asInstanceOf[$tpe])
          override def fromDBObject(c: Any):$tpe = from(c)

          override val entityName: String = ${camelToUnderscores(name.encoded)}
          override val originName: String = ${name.encoded}
          ..$fields

     }"""

  }

}

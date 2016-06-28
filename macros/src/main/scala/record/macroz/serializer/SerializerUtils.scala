package record.macroz.serializer

import record.ReflectionRecord._
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Created by tim on 12.06.16.
 */
object SerializerUtils {

  def getFieldNamesAndTypes(c: Context)(tpe: c.universe.Type):
  Iterable[(String, c.universe.TermName, c.universe.Type)] = {
    import c.universe._

    val fields = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramLists.head

    fields.map { field =>
      field.annotations.find(a => a.toString startsWith "record.macroz.serializer.entityNames")
        .flatMap(_.tree.children.tail.headOption) match {
          case Some(x) =>
            val Literal(Constant(name)) = x
            (name.toString, TermName(field.name.toString.trim), field.typeSignature)
          case None =>  (field.name.toString.trim, TermName(field.name.toString.trim), field.typeSignature)
        }
    }

  }

  def fieldGenerator(c: Context)(parentTpe: c.universe.Type, name: c.universe.Name, tpe: c.universe.Type): c.Tree = {
    import c.universe._

    val fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (entity, name, typ) = p
      fieldGenerator(c)(parentTpe, name, typ)
    }.toList

    val asDBObjectBody = DBObjectSerializer.asDBObject(c)(tpe, q"root")
    val fromBDObjectBody = DBObjectSerializer.fromDBObject(c)(tpe, q"c")

    q"""object ${TermName(name.encoded)} extends record.MacroField[$parentTpe, $tpe](this) {
          override def fromDBObject(c: Any): Any = {
            $fromBDObjectBody
          }
          override def asDBObject(c: Any): Any = {
            val root = c.asInstanceOf[$tpe]
            $asDBObjectBody
          }
          override val originName: String = ${name.encoded}
          override val entityName: String = ${camelToUnderscores(name.encoded)}
       ..$fields
       }"""

  }

  def metaGenerator[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val tpe = weakTypeOf[T]

    val originName = tpe.typeSymbol.name.encoded
    val entityName = camelToUnderscores(originName)

    val fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (entity, name, typ) = p
      fieldGenerator(c)(tpe, name, typ)
    }.toList

    val asBDObjectBody = DBObjectSerializer.asDBObject(c)(tpe, q"root")
    val fromBDObjectBody = DBObjectSerializer.fromDBObject(c)(tpe, q"c")

    q"""new record.MetaTag[$tpe] {
        override def asDBObject(c: Any):Any =  {
          val root = c.asInstanceOf[$tpe]
          $asBDObjectBody
        }
        override def fromDBObject(c: Any):$tpe = {
          $fromBDObjectBody
        }
        override val entityName:String = $entityName
        override val originName:String = $originName
       ..$fields
       }"""

  }

}

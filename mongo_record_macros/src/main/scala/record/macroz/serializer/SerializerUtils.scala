package record.macroz.serializer

import record.ReflectionRecord._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Created by tim on 12.06.16.
 */
object SerializerUtils {

  def getFieldNamesAndTypes(c: whitebox.Context)(tpe: c.universe.Type):
  Iterable[(String, c.universe.TermName, c.universe.Type)] = {
    import c.universe._

    tpe.typeSymbol match {
      case symbol if symbol.isClass && symbol.asClass.isCaseClass =>
        tpe.decls.collectFirst {
          case m: MethodSymbol if m.isPrimaryConstructor => m
        } match {
          case Some(x) => x.paramLists.head.map(field => (getMongoKey(c)(field), TermName(field.name.toString.trim), field.typeSignature))
          case None => Nil
        }
      case _ => Nil
    }

  }

  def getMongoKey(c: whitebox.Context)(field: c.universe.Symbol):String = {
    import c.universe._
    field.annotations.find(a => a.toString startsWith "record.entityName")
      .flatMap(_.tree.children.tail.headOption) match {
      case Some(Literal(Constant(name)))  => name.toString
      case None                           => camelToUnderscores(field.name.toString.trim)
    }
  }

  def getMongoKeyFromModes(c: whitebox.Context)(mods: c.universe.Modifiers):Option[String] = {
    import c.universe._
    mods.annotations.collect {
      case q"new entityName(name = ${Literal(Constant(name))})" => name.toString
    }.headOption
  }

  def fieldGenerator(c: whitebox.Context)(parentTpe: c.universe.Type, name: c.universe.Name, tpe: c.universe.Type, entityName: String): c.Tree = {
    import c.universe._

    val fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (entity, name, typ) = p
      fieldGenerator(c)(parentTpe, name, typ, entity)
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
          override val entityName: String = $entityName
       ..$fields
       }"""

  }

  def metaGenerator[T: c.WeakTypeTag](c: whitebox.Context) = {
    import c.universe._

    val tpe = weakTypeOf[T]

    val entityName = getMongoKey(c)(tpe.typeSymbol)

    val fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (entity, name, typ) = p
      fieldGenerator(c)(tpe, name, typ, entity)
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
       ..$fields
       }"""

  }

}

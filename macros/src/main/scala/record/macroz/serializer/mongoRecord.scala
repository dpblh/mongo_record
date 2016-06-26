package record.macroz.serializer

import record.macroz.paradise.SerializerUtils._

import scala.language.experimental.macros
import scala.annotation.compileTimeOnly
import scala.reflect.macros._
import scala.annotation.StaticAnnotation
import record.ReflectionRecord._

/**
 * Created by tim on 25.06.16.
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class mongoRecord extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro mongoRecordImpl.impl
}

object mongoRecordImpl {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def modifiedDeclaration(classDef: ClassDef, compDecl: Option[ModuleDef] = None): c.Expr[Any] = {
      val className = classDef.name.toTermName

      val originName = classDef.name.encoded
      val entityName = camelToUnderscores(originName)

      val fields = getFieldNamesAndTypes(c)(classDef).map { p =>
        val (name, typ) = p
        fieldGenerator(c)(classDef, name, typ)
      }

      val comp =
        q"""
            object $className extends record.MetaTag[${classDef.name}] {
              import record.macroz.serializer.DBObjectSerializer.{as => asDBO, from => fromDBO}

              private val as = asDBO[${classDef.name}]
              private val from = fromDBO[${classDef.name}]

              override def asDBObject(c: Any):Any = as(c.asInstanceOf[${classDef.name}])
              override def fromDBObject(c: Any):${classDef.name} = from(c)

              override val entityName: String = $entityName
              override val originName: String = $originName
              ..$fields
            }
        """

      c.Expr(
        q"""
          $classDef
          $comp
        """
      )
    }

    annottees.map(_.tree) match {
      case (classDecl: ClassDef) :: Nil => modifiedDeclaration(classDecl)
      case (classDecl: ClassDef) :: (compDecl: ModuleDef) :: Nil => modifiedDeclaration(classDecl, Some(compDecl))
      case _ => c.abort(c.enclosingPosition, "Invalid annottee")
    }

  }
}

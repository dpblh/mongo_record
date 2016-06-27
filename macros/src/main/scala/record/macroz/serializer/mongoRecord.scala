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

    def modifiedDeclaration(classDef: ClassDef, compDeclOpt: Option[ModuleDef] = None): c.Expr[Any] = {
      val className = classDef.name.toTermName

      val originName = classDef.name.encoded
      val entityName = camelToUnderscores(originName)

      var fields = getFieldNamesAndTypes(c)(classDef).map { p =>
        val (name, typ) = p
        fieldGenerator(c)(classDef, name, typ)
      }

      fields = fields ++ Seq(
        q"private val as = asDBO[${classDef.name}]",
        q"private val from = fromDBO[${classDef.name}]",

        q"override def asDBObject(c: Any):Any = as(c.asInstanceOf[${classDef.name}])",
        q"override def fromDBObject(c: Any):${classDef.name} = from(c)",

        q"override val originName: String = $originName",
        q"override val entityName: String = $entityName"
      )

      val compDecl = compDeclOpt map { compDecl =>
        val q"object $obj extends ..$bases { ..$body }" = compDecl
        val extractor = q"object C extends record.MetaTag[${classDef.name}]"
        val q"object C extends $rec" = extractor

        q"""
            object $obj extends $rec {
              import record.macroz.serializer.DBObjectSerializer.{as => asDBO, from => fromDBO}
              ..$body
              ..$fields
            }
          """
      } getOrElse {
        q"""
            object $className extends record.MetaTag[${classDef.name}] {
              import record.macroz.serializer.DBObjectSerializer.{as => asDBO, from => fromDBO}
              ..$fields
            }
           """
      }


      val q"case class $cln(..$params) extends ..$bases { ..$body }" = classDef

      val newC =
        q"""case class $cln(..$params) extends ..$bases {
            ..$body
            def save() = $className.insert(this)
           }"""

      c.Expr(
        q"""
          $newC
          $compDecl
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

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

      val q"$mods class $cln(..$params) extends ..$bases { ..$body }" = classDef
      val originName = cln.encodedName.toString
      val className = cln.toTermName

      val objects = params.map { p =>
        val tpe2 = c.typecheck(tree = q"??? : ${p.tpt}", withMacrosDisabled = true).tpe
        record.macroz.serializer.SerializerUtils.fieldGenerator2(c)(classDef.name.toTypeName, p.name, tpe2)
      }

      val entityName = mods.annotations.collect {
        case q"new entityNames(name = ${Literal(Constant(name))})" => name.toString
      }.headOption match {
        case Some(x) => x
        case None => camelToUnderscores(originName)
      }

      val texFields = Seq(
        q"private val as = asDBO[${classDef.name}]",
        q"private val from = fromDBO[${classDef.name}]",

        q"override def asDBObject(c: Any):Any = as(c.asInstanceOf[${classDef.name}])",
        q"override def fromDBObject(c: Any):${classDef.name} = from(c)",

        q"override val originName: String = $originName",
        q"override val entityName: String = $entityName"
      )

      val compDecl = compDeclOpt map { compDecl =>

        val q"object $obj extends ..$_ { ..$body }" = compDecl

        q"""
            object $obj extends ${AppliedTypeTree(Select(Ident(TermName("record")), TypeName("MetaTag")), List(Ident(classDef.name)))} {
              import record.macroz.serializer.DBObjectSerializer.{as => asDBO, from => fromDBO}
              ..$body
              ..$texFields
              ..$objects
            }
          """
      } getOrElse {
        q"""
            object $className extends record.MetaTag[${classDef.name}] {
              import record.macroz.serializer.DBObjectSerializer.{as => asDBO, from => fromDBO}
              ..$texFields
              ..$objects
            }
           """
      }


      val classDecl =
        q"""$mods class $cln(..$params) extends ..$bases {
            ..$body
            def save() = $className.insert(this)
           }"""

      c.Expr(
        q"""
          $classDecl
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
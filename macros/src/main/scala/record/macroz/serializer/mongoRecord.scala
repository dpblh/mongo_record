package record.macroz.serializer

import record.ReflectionRecord._

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros._

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
        fieldGenerator(c)(classDef.name.toTypeName, p.name, tpe2)
      }

      val entityName = mods.annotations.collect {
        case q"new entityName(name = ${Literal(Constant(name))})" => name.toString
      }.headOption match {
        case Some(x) => x
        case None => camelToUnderscores(originName)
      }

      val texFields = Seq(
        q"private val as = mongo.as[${classDef.name}]",
        q"private val from = mongo.from[${classDef.name}]",

        q"override def asDBObject(c: Any):Any = as(c.asInstanceOf[${classDef.name}])",
        q"override def fromDBObject(c: Any):${classDef.name} = from(c)",

        q"override val originName: String = $originName",
        q"override val entityName: String = $entityName"
      )

      val compDecl = compDeclOpt map { compDecl =>

        val q"object $obj extends ..$_ { ..$body }" = compDecl

        q"""
            object $obj extends ${AppliedTypeTree(Select(Ident(TermName("record")), TypeName("MetaTag")), List(Ident(classDef.name)))} {
              ..$body
              ..$texFields
              ..$objects
            }
          """
      } getOrElse {
        q"""
            object $className extends record.MetaTag[${classDef.name}] {
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

  def fieldGenerator(c: whitebox.Context)(parentTpe: c.universe.TypeName, name: c.universe.Name, tpe: c.universe.Type): c.Tree = {
    import c.universe._

    val fields = SerializerUtils.getFieldNamesAndTypes(c)(tpe).map { p =>
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

}

package record.macroses.serializer

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Created by tim on 10.06.16.
 */
object DBObjectSerializer {

  import record.ReflectionRecord._

  trait Reader[T] {
    def asString(c: T):String
    def asDBObject(c: T):com.mongodb.DBObject
  }

  def DBOGenerator[T]: Reader[T] = macro DBOGeneratorImpl[T]

  def DBOGeneratorImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val tpe = weakTypeOf[T]
    q"""
       new Reader[$tpe] {
          def asDBObject(c: $tpe):com.mongodb.DBObject = ${asDBObject(c)(tpe, q"c")}
          def asString(c: $tpe):String = asDBObject(c).toString
       }
     """
  }

  def getFieldNamesAndTypes(c: Context)(tpe: c.universe.Type):
  Iterable[(c.universe.TermName, c.universe.Type)] = {
    import c.universe._

    object CaseField {
      def unapply(trmSym: TermSymbol): Option[(TermName, Type)] = {
        if (trmSym.isVal && trmSym.isCaseAccessor)
          Some((TermName(trmSym.name.toString.trim), trmSym.typeSignature))
        else
          None
      }
    }

    tpe.decls.collect {
      case CaseField(nme, tpe) =>
        (nme, tpe)
    }
  }

  def asDBObject(c: Context)(tpe: c.universe.Type, name: c.Tree): c.Tree = {
    import c.universe._
    tpe match {
      case x if scalaz.isSimpleType(c)(tpe) => q"${mongo.asSimpleType(c)(tpe, name)}"
      case x if scalaz.isDate(c)(tpe) => q"${mongo.asDate(c)(tpe, name)}"
      case x if scalaz.isOption(c)(tpe) => q"${mongo.asOption(c)(tpe, name)}"
      case x if scalaz.isMap(c)(tpe) => q"${mongo.asMap(c)(tpe, name)}"
      case x if scalaz.isCollection(c)(tpe) => q"${mongo.asCollection(c)(tpe, name)}"
      case x => q"${mongo.asCase(c)(tpe, name)}"
    }
  }

  def fieldGenerator(c: Context)(parentTpe: c.universe.Type, name: c.universe.Name, tpe: c.universe.Type): c.Tree = {
    import c.universe._

    val fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (name, typ) = p
      fieldGenerator(c)(parentTpe, name, typ)
    }.toList

    q"""object ${TermName(name.encoded)} extends record.Field[$parentTpe, $tpe] {
          override val collection: record.Make[$parentTpe] = self
          override def fromDBObject(dbo: Any): Any = ???
          override def asDBObject(c: Any): Any = ???
          override val originName: String = ${name.encoded}
          override val entityName: String = ${camelToUnderscores(name.encoded)}
       ..$fields
       }"""

  }

  def metaGenerator[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val tpe = weakTypeOf[T]

    val collection_name = camelToUnderscores(tpe.typeSymbol.name.toString)

    val fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (name, typ) = p
      fieldGenerator(c)(tpe, name, typ)
    }.toList

    val asBDObjectBody = asDBObject(c)(tpe, q"root")

    q"""new Meta[$tpe] { self =>
        val collection_name = $collection_name
        override def asDBObject(c: Any):Any =  {
          val root = c.asInstanceOf[$tpe]
          $asBDObjectBody
        }
       ..$fields
       }"""

  }

}
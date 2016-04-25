package record

import scala.reflect.macros.whitebox.Context

/**
 * Created by tim on 08.04.16.
 */
trait UtilsMacro {
  def getFieldNamesAndTypes(c: Context)(tpe: c.universe.Type):
  Iterable[(c.universe.Name, c.universe.Type)] = {
    import c.universe._

    object CaseField {
      def unapply(trmSym: TermSymbol): Option[(Name, Type)] = {
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

  def camelToUnderscores(name: String) = name.replaceAll("(.)(\\p{Upper})", "$1_$2").toLowerCase

  def isSimpleType(c: Context)(tpe: c.universe.Type):Boolean = {
    import c.universe._
    tpe.typeSymbol.name.toString match {
      case "Int" => true
      case "String" => true
      case "Long" => true
      case _ => false
    }
  }
}

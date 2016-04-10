package query

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
          Some((newTermName(trmSym.name.toString.trim), trmSym.typeSignature))
        else
          None
      }
    }

    tpe.declarations.collect {
      case CaseField(nme, tpe) =>
        (nme, tpe)
    }
  }
}

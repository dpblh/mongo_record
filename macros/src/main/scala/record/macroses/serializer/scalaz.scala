package record.macroses.serializer

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Created by tim on 10.06.16.
 */
object scalaz {

  def isCollection(c: Context)(tpe: c.universe.Type): Boolean = collections.contains(tpe.typeSymbol.name.toString)

  def isMap(c: Context)(tpe: c.universe.Type): Boolean = {
    import c.universe._
    tpe match {
      case q"Map" => true
      case _ => false
    }
  }

  def isOption(c: Context)(tpe: c.universe.Type): Boolean = {
    import c.universe._
    tpe match {
      case q"Option" => true
      case _ => false
    }
  }

  def isDate(c: Context)(tpe: c.universe.Type): Boolean = dates.contains(tpe.typeSymbol.name.toString)

  def isSimpleType(c: Context)(tpe: c.universe.Type): Boolean = simpleType.contains(tpe.typeSymbol.name.toString)

  val simpleType = Set("Int", "String", "Long", "Double", "Float", "Byte", "Byte", "BigInt", "BigDecimal", "Boolean", "Array[Byte]")
  val dates = Set("Date", "Calendar")
  val collections = Set("List", "Set", "Seq")
}

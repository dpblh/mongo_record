package record.macroz.serializer

import record.macroz.serializer.SerializerUtils._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Created by tim on 10.06.16.
 */
object scalaz {

  def isCase(c: Context)(tpe: c.universe.Type): Boolean = getFieldNamesAndTypes(c)(tpe).nonEmpty

  def isCollection(c: Context)(tpe: c.universe.Type): Boolean = collections.contains(tpe.typeSymbol.name.toString)

  def isMap(c: Context)(tpe: c.universe.Type): Boolean = {
    tpe match {
      case x if x.typeSymbol.fullName.toString == "scala.collection.immutable.Map" => true
      case _ => false
    }
  }

  def isOption(c: Context)(tpe: c.universe.Type): Boolean = {
    tpe match {
      case x if x.typeSymbol.fullName.toString == "scala.Option" => true
      case _ => false
    }
  }

  def isDate(c: Context)(tpe: c.universe.Type): Boolean = dates.contains(tpe.typeSymbol.name.toString)

  def isSimpleType(c: Context)(tpe: c.universe.Type): Boolean = {
    tpe match {
      case x if simpleType.contains(x.typeSymbol.fullName.toString) => true
      case x if x.typeSymbol.fullName.toString == "scala.Array" && x.typeArgs.head.typeSymbol.fullName.toString == "scala.Byte" => true
      case _ => false
    }
  }

  val simpleType = Set("scala.Int", "java.lang.String", "scala.Long", "scala.Double", "scala.Float", "scala.Byte", "scala.math.BigInt", "scala.math.BigDecimal", "scala.Boolean")
  val dates = Set("Date", "Calendar")
  val collections = Set("List", "Set", "Seq")
}

package record.macroz.serializer

import record.macroz.serializer.DBObjectSerializer._
import record.macroz.serializer.SerializerUtils._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Created by tim on 10.06.16.
 */
object scalaz {

  def isCase(c: Context)(tpe: c.universe.Type): Boolean = getFieldNamesAndTypes(c)(tpe).nonEmpty

  def isCollection(c: Context)(tpe: c.universe.Type): Boolean = collections.contains(tpe.typeSymbol.fullName.toString)

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

  def isDate(c: Context)(tpe: c.universe.Type): Boolean = dates.contains(tpe.typeSymbol.fullName.toString)

  def isSimpleType(c: Context)(tpe: c.universe.Type): Boolean = {
    tpe match {
      case x if simpleType.contains(x.typeSymbol.fullName.toString) => true
      case x if x.typeSymbol.fullName.toString == "scala.Array" && x.typeArgs.head.typeSymbol.fullName.toString == "scala.Byte" => true
      case _ => false
    }
  }

  def asSimpleType(c: Context)(tpe: c.universe.Type, name: c.Tree):c.Tree = {
    import c.universe._
    tpe match {
      case x if x.typeSymbol.fullName.toString == "scala.Int"             => q"$name.asInstanceOf[java.lang.Integer].toInt"
      case x if x.typeSymbol.fullName.toString == "scala.Float"           => q"$name.asInstanceOf[Double].toFloat"
      case x if x.typeSymbol.fullName.toString == "scala.Byte"            => q"$name.toString.toByte"
      case x if x.typeSymbol.fullName.toString == "scala.math.BigDecimal" => q"BigDecimal($name.toString)"
      case x if x.typeSymbol.fullName.toString == "scala.math.BigInt"     => q"BigInt($name.toString)"
      case _ => name
    }
  }
  def asDate(c: Context)(tpe: c.universe.Type, name: c.Tree):c.Tree = {
    import c.universe._
    val milis = q"""
         $name match {
          case x: BigDecimal  => x.longValue()
          case x: Double      => x.toLong
          case x              => x.toString.toLong
         }
     """
    tpe match {
      case x if x.typeSymbol.fullName.toString == "java.util.Date"      => q"new java.util.Date($milis)"
      case x if x.typeSymbol.fullName.toString == "java.util.Calendar"  => q"record.UtilsRecord.asCalendar($milis)"
    }
  }
  def asOption(c: Context)(tpe: c.universe.Type, name: c.Tree):c.Tree = {
    import c.universe._
    q"""
       $name match {
        case null => None
        case x    => Some(${fromDBObject(c)(tpe.typeArgs.head, q"x")})
       }
     """
  }
  def asCollection(c: Context)(tpe: c.universe.Type, name: c.Tree):c.Tree = {
    import c.universe._
    val collection = tpe match {
      case x if x.typeSymbol.fullName.toString == "scala.collection.immutable.List"   => q"$name.toList"
      case x if x.typeSymbol.fullName.toString == "scala.collection.immutable.Set"    => q"$name.toSet"
      case x if x.typeSymbol.fullName.toString == "scala.collection.immutable.Seq"    => q"$name.toSeq"
      case x                                                                          => throw DBObjectSerializerException("Unsupported collection type %s".format(x.typeSymbol.fullName.toString))
    }
    q"$collection.map { element => ${fromDBObject(c)(tpe.typeArgs.head, q"element")} }"
  }
  def asClass(c: Context)(tpe: c.universe.Type, root: c.Tree):c.Tree = {
    import c.universe._
    val companion = tpe.typeSymbol.companionSymbol

    val caseParams = getFieldNamesAndTypes(c)(tpe) map { tupl =>
      val (name, returnType) = tupl
      q"""$root.asInstanceOf[com.mongodb.BasicDBObject].get(${name.decoded}).asInstanceOf[$returnType]"""
    }
    q"$companion(..$caseParams)"
  }

  val simpleType = Set("scala.Int", "java.lang.String", "scala.Long", "scala.Double", "scala.Float", "scala.Byte", "scala.math.BigInt", "scala.math.BigDecimal", "scala.Boolean")
  val dates = Set("java.util.Date", "java.util.Calendar")
  val collections = Set("scala.collection.immutable.List", "scala.collection.immutable.Set", "scala.collection.Seq")
}

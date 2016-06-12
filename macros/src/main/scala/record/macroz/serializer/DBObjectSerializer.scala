package record.macroz.serializer

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Created by tim on 10.06.16.
 */
object DBObjectSerializer {

  case class DBObjectSerializerException(msg: String) extends RuntimeException(msg)

  def fromDBObject(c: Context)(tpe: c.universe.Type, name: c.Tree): c.Tree = {
    import c.universe._
    tpe match {
      case x if scalaz.isSimpleType(c)(x)     => q"${scalaz.asSimpleType(c)(x, name)}"
      case x if scalaz.isDate(c)(x)           => q"${scalaz.asDate(c)(x, name)}"
      case x if scalaz.isOption(c)(x)         => q"${scalaz.asOption(c)(x, name)}"
      case x if scalaz.isCollection(c)(x)     => q"${scalaz.asCollection(c)(x, name)}"
      case x if scalaz.isCase(c)(x)           => q"${scalaz.asClass(c)(x, name)}"
      case x                                  => throw DBObjectSerializerException(s"Error deserialize from ${tpe.typeSymbol.toString}")
    }
  }

  def asDBObject(c: Context)(tpe: c.universe.Type, name: c.Tree): c.Tree = {
    import c.universe._
    tpe match {
      case x if scalaz.isSimpleType(c)(tpe)       => q"${mongo.asSimpleType(c)(tpe, name)}"
      case x if scalaz.isDate(c)(tpe)             => q"${mongo.asDate(c)(tpe, name)}"
      case x if scalaz.isOption(c)(tpe)           => q"${mongo.asOption(c)(tpe, name)}"
      case x if scalaz.isMap(c)(tpe)              => q"${mongo.asMap(c)(tpe, name)}"
      case x if scalaz.isCollection(c)(tpe)       => q"${mongo.asCollection(c)(tpe, name)}"
      case x if scalaz.isCase(c)(tpe)             => q"${mongo.asCase(c)(tpe, name)}"
      case x                                      => throw DBObjectSerializerException("Unsupported type %s".format(x))
    }
  }

  def asDBObjectImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val tpe = weakTypeOf[T]
    q"(c: $tpe) => ${asDBObject(c)(tpe, q"c")}"
  }

  def fromDBObjectImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val tpe = weakTypeOf[T]
    q"(c: Any) => ${fromDBObject(c)(tpe, q"c")}"
  }

}
package record.serializer

import com.mongodb._

import scala.reflect.runtime.universe._

/**
 * Created by tim on 11.05.16.
 */
object DBObjectSerializer {

  case class DBObjectSerializerException(msg: String) extends RuntimeException(msg)

  def fromDBObject[T: TypeTag](m: DBObject):Any = fromDBObject(m, typeOf[T])
  def fromDBObject(value: Any, tpe: Type):Any = {
    tpe match {
      case x if scalaz.isSimpleType(x)    => scalaz.asSimpleType(value, x)
      case x if scalaz.isDate(x)          => scalaz.asDate(x, value)
      case x if scalaz.isOption(x)        => scalaz.asOption(x, value)
      case x if scalaz.isCollection(x)    => scalaz.asCollection(x, value)
      case x =>
        value match {
          case y: BasicDBObject           => scalaz.asClass(x, y)
          case _                          => throw DBObjectSerializerException(s"Error deserialize from ${tpe.typeSymbol.toString}: value $value")
        }
    }

  }

  def asDBObject[A: TypeTag](entity: A):Any = asDBObject(entity, typeOf[A])
  def asDBObject[T](value: T, tup: Type):Any = tup match {
    case x if scalaz.isSimpleType(x)  => mongo.asSimpleType(value)
    case x if scalaz.isDate(x)        => mongo.asDate(value)
    case x if scalaz.isOption(x)      => mongo.asOption(value, x)
    case x if scalaz.isMap(x)         => mongo.asMap(value, x)
    case x if scalaz.isCollection(x)  => mongo.asCollection(value, x)
    case x if scalaz.isCase(x)        => mongo.asCase(value, x)
    case x                            => throw DBObjectSerializerException("Unsupported type %s %s".format(x, value))
  }

}
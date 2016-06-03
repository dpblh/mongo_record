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
  def asDBObject[T](entity: T, tup: Type):Any = {
    val mirror = runtimeMirror(entity.getClass.getClassLoader)

    def a2dbObject(x: Any, t: Type): Any = {
      val xm = mirror reflect x
      val members = t.decls.collect {
        case acc: MethodSymbol if acc.isCaseAccessor => fieldAsTuple(acc, xm)
      }

      if (members.isEmpty) {
        x match {
          case x1 if scalaz.isMap(t) => mongo.asMap(x1, t)
          case _                        => x
        }
      } else {
        val builder = BasicDBObjectBuilder.start()

        members.foreach { a => builder.append(a._1, a._2) }
        builder.get()
      }
    }

    def fieldAsTuple(acc: MethodSymbol, xm: InstanceMirror):(String, Any) = {
      val value = (xm reflectMethod acc)()
      val returnValue = acc.returnType match {

        case x if scalaz.isSimpleType(x)  => mongo.asSimpleType(value)
        case x if scalaz.isDate(x)        => mongo.asDate(value)
        case x if scalaz.isOption(x)      => mongo.asOption(value, x)
        case x if scalaz.isCollection(x)  => mongo.asCollection(value, x)
        case x                            => a2dbObject(value, acc.typeSignature)

      }
      (acc.name.decodedName.toString, returnValue)
    }

    a2dbObject(entity, tup)

  }

}
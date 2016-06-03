package record

import java.util.{Calendar, Date}
import com.mongodb._
import scala.collection.JavaConversions._
import scala.reflect.runtime.universe._

/**
 * Created by tim on 11.05.16.
 */
object DBObjectSerializer {

  case class DBObjectSerializerException(msg: String) extends RuntimeException(msg)

  def fromDBObject[T: TypeTag](m: DBObject):Any = fromDBObject(m, typeOf[T])

  def asObject[R](value: Any, tpe: Type):R = fromDBObject(value, tpe).asInstanceOf[R]

  def fromDBObject(value: Any, tpe: Type):Any = {
    tpe match {
      case x if scalaz.isSimpleType(x)    => scalaz.asSimpleType(value, x)
      case x if scalaz.isDate(x)          => scalaz.asDate(x, value)
      case x if scalaz.isOption(x)        => scalaz.asOption(x, value)
      case x =>
        value match {
          case y: BasicDBList => scalaz.asCollection(y, x)
          case y: BasicDBObject =>
            val rm = runtimeMirror(getClass.getClassLoader)
            val classTest = tpe.typeSymbol.asClass
            val classMirror = rm.reflectClass(classTest)
            val constructor = tpe.decl(termNames.CONSTRUCTOR).asMethod
            val constructorMirror = classMirror.reflectConstructor(constructor)
            val constructorArgs = constructor.paramLists.flatten.map { param =>
              val name = param.name.toString
              val value = y.get(name)
              fromDBObject(value, param.typeSignature)
            }
            constructorMirror(constructorArgs: _*)
          case _ => throw DBObjectSerializerException(s"Error deserialize from ${tpe.typeSymbol.toString}: value $value")
        }
    }

  }

  def asDBObject[T](entity: T, tup: Type):Any = {
    val mirror = runtimeMirror(entity.getClass.getClassLoader)

    def a2dbObject(x: Any, t: Type): Any = {
      val xm = mirror reflect x
      val members = t.decls.collect {
        case acc: MethodSymbol if acc.isCaseAccessor => fieldAsTuple(acc, xm)
      }

      if (members.isEmpty) {
        x match {
          case x1 if mongo.isMap(t) =>
            val builder = BasicDBObjectBuilder.start()
            x1.asInstanceOf[Map[String,_]].foreach { tupl =>
              val (key, value) = tupl
              builder.append(key, a2dbObject(value, tup.typeArgs(1)))
            }
            builder.get()
          case _ => x
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

  def asDBObject[A: TypeTag](entity: A):Any = asDBObject(entity, typeOf[A])

  object mongo {
    def asSimpleType(o: Any):Any = {
      o match {
        case x: BigInt              => x.toString()
        case x: BigDecimal          => x.toString()
        case x => x
      }
    }
    def asDate(o: Any):Any          = o match {
      case x: Date                  => x.getTime
      case x: Calendar              => x.getTimeInMillis
    }
    def asOption(o: Any, tup: Type):Any = o match {
      case Some(x)   => asDBObject(x, tup.typeArgs.head)
      case None      => null
    }
    def asCollection(o: Any, tup: Type):Any = {
      val list = new BasicDBList()
      o.asInstanceOf[Iterable[_]].foreach { element =>
        list.add(asDBObject(element, tup.typeArgs.head).asInstanceOf[AnyRef])//TODO AnyRef ?
      }
      list
    }
    def isMap(`type`: Type):Boolean = `type` <:< typeOf[Map[String,_]]
  }

  object scalaz {
    def isSimpleType(`type`: Type): Boolean     = simpleTypes.exists(_ =:= `type`)
    def asSimpleType(o: Any, tup: Type):Any     = tup match {
      case x if x =:= typeOf[Int]               => o.asInstanceOf[java.lang.Integer].toInt
      case x if x =:= typeOf[Float]             => o.asInstanceOf[Double].toFloat
      case x if x =:= typeOf[Byte]              => o.toString.toByte
      case x if x =:= typeOf[BigDecimal]        => BigDecimal(o.toString)
      case x if x =:= typeOf[BigInt]            => BigInt(o.toString)
      case x: CharSequence                      => x.toString
      //Long, String, Double, Boolean, Array[Byte]
      case _                                    => o
    }
    def isDate(`type`: Type): Boolean           = dates.exists(_ =:= `type`)
    def asDate(`type`: Type, o: Any):Any        = {
      val milis = o match {
        case x: BigDecimal  => x.longValue()
        case x: Double      => x.toLong
        case x              => x.toString.toLong
      }
      `type` match {
        case x if x =:= typeOf[Date]              => new Date(milis)
        case x if x =:= typeOf[Calendar]          => UtilsRecord.asCalendar(milis)
      }
    }
    def isCollection(`type`: Type):Boolean = `type` <:< typeOf[Iterable[_]]
    def asCollection(o: BasicDBList, tup: Type):Any = {
      val collection = tup match {
        case x if tup <:< typeOf[List[_]] => o.toList
        case x if tup <:< typeOf[Set[_]]=> o.toSet
        case x if tup <:< typeOf[Seq[_]]=> o.toList
        case x => throw DBObjectSerializerException("Unsupported collection type %s".format(tup.toString))
      }
      collection.map( o => fromDBObject(o, tup.typeArgs.head))
    }
    def isOption(`type`: Type): Boolean = `type` <:< typeOf[Option[Any]]
    def asOption(`type`: Type, o: Any):Any = o match {
      case null     =>  None
      case x        =>  Some(fromDBObject(x, `type`.typeArgs.head))
    }

    val simpleTypes = Set[Type](typeOf[String], typeOf[Int], typeOf[Long], typeOf[Double],
      typeOf[Float], typeOf[Byte], typeOf[BigInt], typeOf[BigDecimal], typeOf[Boolean],
      typeOf[Array[Byte]])

    val dates = Set(typeOf[Date], typeOf[Calendar])

  }

}
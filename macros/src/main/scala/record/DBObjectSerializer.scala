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

  def fromDBObject[T: TypeTag](m: DBObject):Any = fromDBObjectType(m, typeOf[T])

  def fromDBObjectType(value: Any, tpe: Type):Any = {
    tpe match {
      case x if isPrimitive(x) => dbPrimitive2primitive(value, x)
      case x if isDate(x) => any2date(x, value)
      case x =>
        value match {
          case y: BasicDBList => basicDBList2list(y, x)
          case y: BasicDBObject =>
            val rm = runtimeMirror(getClass.getClassLoader)
            val classTest = tpe.typeSymbol.asClass
            val classMirror = rm.reflectClass(classTest)
            val constructor = tpe.decl(termNames.CONSTRUCTOR).asMethod
            val constructorMirror = classMirror.reflectConstructor(constructor)
            val constructorArgs = constructor.paramLists.flatten.map { param =>
              val name = param.name.toString
              val value = y.get(name)
              fromDBObjectType(value, param.typeSignature)
            }
            constructorMirror(constructorArgs: _*)
          case _ => throw DBObjectSerializerException(s"Error deserialize from ${tpe.typeSymbol.toString}: value $value")
        }
    }

  }

  def basicDBList2list(o: BasicDBList, tup: Type):Any = {
    val collection = tup match {
      case x if tup <:< typeOf[List[_]] => o.toList
      case x if tup <:< typeOf[Set[_]]=> o.toSet
      case x if tup <:< typeOf[Seq[_]]=> o.toList
    }
    collection.map( o => fromDBObjectType(o.asInstanceOf[DBObject], tup.typeArgs.head))
  }

  def asDBObjectImplicit[T](entity: T, tup: Type):Any = {
    val mirror = runtimeMirror(entity.getClass.getClassLoader)

    def a2dbObject(x: Any, t: Type): Any = {
      val xm = mirror reflect x
      val members = t.decls.collect {
        case acc: MethodSymbol if acc.isCaseAccessor => fieldAsTuple(acc, xm)
      }

      if (members.isEmpty) x else {
        val builder = BasicDBObjectBuilder.start()

        members.foreach { a => builder.append(a._1, a._2) }
        builder.get()
      }
    }

    def fieldAsTuple(acc: MethodSymbol, xm: InstanceMirror):(String, Any) = {
      val value = (xm reflectMethod acc)()
      val returnValue = acc.returnType match {

        case x if isPrimitive(x) => value
        case x if isDate(x) => any2DBDate(value)
        case x => a2dbObject(value, acc.typeSignature)

      }
      (acc.name.decodedName.toString, returnValue)
    }

    a2dbObject(entity, tup)

  }

  def asDBObject[A: TypeTag](entity: A):Any = asDBObjectImplicit(entity, typeOf[A])

  def isDate(`type`: Type):       Boolean = dates.exists(_ =:= `type`)
  def isPrimitive(`type`: Type):  Boolean = primitives.exists(_ =:= `type`)

  def any2DBDate(o: Any):Any = o match {
    case x: Date     => x.getTime
    case x: Calendar => x.getTimeInMillis
  }

  def dbPrimitive2primitive(o: Any, tup: Type):Any = {
    val typeOfInt = typeOf[Int]
    tup match {
      case `typeOfInt` =>
        o match {
          case y: BigDecimal => y.intValue()
          case y: Double => y.intValue()
          case y: java.lang.Integer => y.toInt
        }
      case x: CharSequence => x.toString
      case _ => o
    }
  }

  def any2date(`type`: Type, o: Any):Any = {
    val milis = o match {
      case y: BigDecimal => y.toLong
      case y: Double => y.toLong
    }
    `type` match {
      case x if x =:= typeOf[Date] => new Date(milis)
      case x if x =:= typeOf[Calendar] =>
        val date = Calendar.getInstance()
        date.setTimeInMillis(milis)
        date
    }
  }

  val lists = Set(typeOf[List[_]], typeOf[Set[_]], typeOf[Seq[_]])
  val dates = Set(typeOf[Date], typeOf[Calendar])
  val primitives = Set[Type](typeOf[String], typeOf[Int], typeOf[Long], typeOf[Double],
    typeOf[Float], typeOf[Byte], typeOf[BigInt], typeOf[Boolean],
    typeOf[Short], typeOf[java.lang.Integer], typeOf[java.lang.Long],
    typeOf[java.lang.Double], typeOf[java.lang.Float],
    typeOf[java.lang.Byte], typeOf[java.lang.Boolean],
    typeOf[java.lang.Short], typeOf[scala.Array[Byte]])


}
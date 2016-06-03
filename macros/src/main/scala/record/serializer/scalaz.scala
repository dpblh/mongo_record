package record.serializer

import java.util.{Calendar, Date}

import com.mongodb.{BasicDBObject, BasicDBList}
import record.UtilsRecord
import record.serializer.DBObjectSerializer._

import scala.collection.JavaConversions._
import scala.reflect.runtime.universe._
/**
 * Created by tim on 03.06.16.
 */
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

  def asDate(`type`: Type, o: Any):Any  = {
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

  def asCollection(tup: Type, o: Any):Any = {
    val dbo = o.asInstanceOf[BasicDBList]
    val collection = tup match {
      case x if tup <:< typeOf[List[_]]   => dbo.toList
      case x if tup <:< typeOf[Set[_]]    => dbo.toSet
      case x if tup <:< typeOf[Seq[_]]    => dbo.toList
      case x => throw DBObjectSerializerException("Unsupported collection type %s".format(tup.toString))
    }
    collection.map( o => fromDBObject(o, tup.typeArgs.head))
  }

  def isOption(`type`: Type): Boolean = `type` <:< typeOf[Option[Any]]

  def asOption(`type`: Type, o: Any):Any = o match {
    case null     =>  None
    case x        =>  Some(fromDBObject(x, `type`.typeArgs.head))
  }

  def isMap(`type`: Type):Boolean = `type` <:< typeOf[Map[String,_]]

  def isCase(`type`: Type):Boolean = {
    `type`.decls.collect {
      case m: MethodSymbol if m.isCaseAccessor => m.name.toString
    }.nonEmpty
  }
  def asClass(`type`: Type, y: BasicDBObject):Any = {
    val rm = runtimeMirror(getClass.getClassLoader)
    val classTest = `type`.typeSymbol.asClass
    val classMirror = rm.reflectClass(classTest)
    val constructor = `type`.decl(termNames.CONSTRUCTOR).asMethod
    val constructorMirror = classMirror.reflectConstructor(constructor)
    val constructorArgs = constructor.paramLists.flatten.map { param =>
      val name = param.name.toString
      val value = y.get(name)
      fromDBObject(value, param.typeSignature)
    }
    constructorMirror(constructorArgs: _*)
  }

  val simpleTypes = Set[Type](typeOf[String], typeOf[Int], typeOf[Long], typeOf[Double],
    typeOf[Float], typeOf[Byte], typeOf[BigInt], typeOf[BigDecimal], typeOf[Boolean],
    typeOf[Array[Byte]])

  val dates = Set(typeOf[Date], typeOf[Calendar])

}

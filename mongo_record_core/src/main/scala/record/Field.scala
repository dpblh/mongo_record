package record

import record.runtime.serializer.DBObjectSerializer
import record.signatures._
import scala.reflect.runtime.universe._
/**
 * Created by tim on 21.05.16.
 */
trait Field[C, F] extends Make[C] {

  val collection: Make[C]

  def ===[C1](joined: Field[C1, F])           =   JoinOne(this, joined)
  def <<[C1](joined: Field[C1, F])            =   JoinMany(this, joined)

  def ===[B <: F](right: B)   = BooleanExpression(this, right, "$eq")
  def >(right: F)             = BooleanExpression(this, right, "$gt")
  def <(right: F)             = BooleanExpression(this, right, "$lt")
  def >=(right: F)            = BooleanExpression(this, right, "$gte")
  def <=(right: F)            = BooleanExpression(this, right, "$lte")

  override def toString = {
    collection match {
      case x: MetaTag[C] => entityName
      case x: Field[C, _] => x+"."+entityName
    }
  }

}

trait FieldRuntime[C,F] extends Field[C,F] with MakeRuntime[C]

abstract class MacroField[C, F](val collection: Make[C]) extends Field[C, F]

trait ObjectField[C, F] extends FieldRuntime[C, F] {
  val entityName: String  = ReflectionRecord.getNameAsUnderscores(getClass)
}

trait BaseFields {
  //AnyVal
  case class CalendarField[C](collection: Make[C]) extends ObjectField[C, java.util.Calendar]{
    override def asDBObject(c: Any): Any                    = c.toString.toLong
    override def fromDBObject(dbo: Any): java.util.Calendar = UtilsRecord.asCalendar(dbo.toString.toLong)
  }
  case class DateField[C](collection: Make[C]) extends ObjectField[C, java.util.Date]{
    override def asDBObject(c: Any): Any                = c.toString.toLong
    override def fromDBObject(dbo: Any): java.util.Date = new java.util.Date(dbo.toString.toLong)
  }
  case class BigDecimalField[C](collection: Make[C]) extends ObjectField[C, BigDecimal]{
    override def asDBObject(c: Any): Any            = c.toString
    override def fromDBObject(dbo: Any): BigDecimal = BigDecimal(dbo.toString)
  }
  case class BigIntField[C](collection: Make[C]) extends ObjectField[C, BigInt]{
    override def asDBObject(c: Any): Any            = c.toString
    override def fromDBObject(dbo: Any): BigInt     = BigInt(dbo.toString)
  }
  case class ByteArrayField[C](collection: Make[C]) extends ObjectField[C, Array[Byte]]{
    override def asDBObject(c: Any): Any            = c
    override def fromDBObject(dbo: Any): Array[Byte]= dbo.asInstanceOf[Array[Byte]]
  }
  case class ByteField[C](collection: Make[C]) extends ObjectField[C, Byte]{
    override def asDBObject(c: Any): Any            = c.toString.toInt
    override def fromDBObject(dbo: Any): Byte       = dbo.toString.toByte
  }
  case class FloatField[C](collection: Make[C]) extends ObjectField[C, Float]{
    override def asDBObject(c: Any): Any            = c.toString.toDouble
    override def fromDBObject(dbo: Any): Float      = dbo.toString.toFloat
  }
  case class DoubleField[C](collection: Make[C]) extends ObjectField[C, Double]{
    override def asDBObject(c: Any): Any            = c
    override def fromDBObject(dbo: Any): Double     = dbo.toString.toDouble
  }
  case class BooleanField[C](collection: Make[C]) extends ObjectField[C, Boolean]{
    override def asDBObject(c: Any): Any            = c
    override def fromDBObject(dbo: Any): Boolean    = dbo.toString.toBoolean
  }
  case class IntField[C](collection: Make[C]) extends ObjectField[C, Int]{
    override def asDBObject(c: Any): Any            = c
    override def fromDBObject(dbo: Any): Int        = dbo.toString.toInt
  }
  case class LongField[C](collection: Make[C]) extends ObjectField[C, Long] {
    override def asDBObject(c: Any): Any            = c
    override def fromDBObject(dbo: Any): Long       = dbo.toString.toLong
  }
  case class StringField[C](collection: Make[C]) extends ObjectField[C, String] {
    override def asDBObject(c: Any): Any            = c
    override def fromDBObject(dbo: Any): String     = dbo.toString
  }
  //AnyRef
  case class OptionField[C, F](collection: Make[C])(implicit t: TypeTag[F]) extends ObjectField[C, Option[F]]{
    override def asDBObject(c: Any): Any                    = DBObjectSerializer.asDBObject(c, runtimeClass, Some(this))
    override def fromDBObject(dbo: Any): Option[F]          = if (dbo != null) Some(DBObjectSerializer.fromDBObject(dbo, runtimeClass, Some(this)).asInstanceOf[F]) else None
    def runtimeClass: Type = typeOf[F]
  }
  case class InnerField[C, F](collection: Make[C])(implicit t: TypeTag[F]) extends ObjectField[C, F] {
    override def asDBObject(c: Any): Any            = DBObjectSerializer.asDBObject(c, runtimeClass, Some(this))
    override def fromDBObject(dbo: Any): F          = DBObjectSerializer.fromDBObject(dbo, runtimeClass, Some(this)).asInstanceOf[F]
    def runtimeClass: Type = typeOf[F]
  }
}
case class RuntimeField[C, F](override val entityName: String, collection: Make[C])(implicit tuo: TypeTag[F]) extends FieldRuntime[C, F] {
  override def asDBObject(c: Any): Any            = DBObjectSerializer.asDBObject(c, runtimeClass, None)
  override def fromDBObject(dbo: Any): F          = DBObjectSerializer.fromDBObject(dbo, runtimeClass, None).asInstanceOf[F]
  def runtimeClass: Type = typeOf[F]
}

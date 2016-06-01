package record

import record.signatures._
import scala.reflect.runtime.universe._
/**
 * Created by tim on 21.05.16.
 */
trait Field[C, F] extends Make[C] {

  val fieldName: String
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
      case x: MetaTag[C] => fieldName
      case x: Field[C, _] => x+"."+fieldName
    }
  }

}

trait ObjectField[C, F] extends Field[C, F] {
  val fieldName: String = ReflectionRecord.getName(getClass)
}

trait BaseFields {
  //AnyVal
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
  case class InnerField[C, F](collection: Make[C])(implicit t: TypeTag[F]) extends ObjectField[C, F] {
    override def asDBObject(c: Any): Any            = DBObjectSerializer.asDBObject(c, runtimeClass)
    override def fromDBObject(dbo: Any): F          = DBObjectSerializer.fromDBObject(dbo, runtimeClass).asInstanceOf[F]
    def runtimeClass: Type = typeOf[F]
  }
  case class RuntimeField[C, F](override val fieldName: String, collection: Make[C])(implicit tuo: TypeTag[F]) extends Field[C, F] {
    override def asDBObject(c: Any): Any            = DBObjectSerializer.asDBObject(c, runtimeClass)
    override def fromDBObject(dbo: Any): F          = DBObjectSerializer.fromDBObject(dbo, runtimeClass).asInstanceOf[F]
    def runtimeClass: Type = typeOf[F]
  }
}

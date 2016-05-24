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
  val fieldName: String = {
    val fullName = getClass.getName
    val className = fullName.substring(fullName.lastIndexOf(".")+1)
    val simpleName = className.split("\\$")
    simpleName(simpleName.length - 1)
  }
}

trait BaseFields {
  case class StringField[C](collection: Make[C]) extends ObjectField[C, String] {
    override def asDBObject(c: Any): Any        = c
    override def fromDBObject(dbo: Any): Any    = dbo.toString
  }
  case class IntField[C](collection: Make[C]) extends ObjectField[C, Int]{
    override def asDBObject(c: Any): Any        = c
    override def fromDBObject(dbo: Any): Any    = dbo.toString.toInt
  }
  case class LongField[C](collection: Make[C]) extends ObjectField[C, Long] {
    override def asDBObject(c: Any): Any        = c
    override def fromDBObject(dbo: Any): Any    = dbo.toString.toLong
  }
  case class InnerField[C, F](collection: Make[C])(implicit t: TypeTag[F]) extends ObjectField[C, F] {
    override def asDBObject(c: Any): Any        = DBObjectSerializer.asDBObject(c, runtimeClass)
    override def fromDBObject(dbo: Any): Any    = DBObjectSerializer.fromDBObject(dbo, runtimeClass)
    def runtimeClass: Type = typeOf[F]
  }
  case class RuntimeField[C, F](override val fieldName: String, collection: Make[C])(implicit tuo: TypeTag[F]) extends Field[C, F] {
    override def asDBObject(c: Any): Any        = DBObjectSerializer.asDBObject(c, runtimeClass)
    override def fromDBObject(dbo: Any): Any    = DBObjectSerializer.fromDBObject(dbo, runtimeClass)
    def runtimeClass: Type = typeOf[F]
  }
}

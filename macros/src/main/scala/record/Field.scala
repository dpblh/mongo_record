package record

import record.signatures._
import scala.reflect.runtime.universe._
/**
 * Created by tim on 21.05.16.
 */
trait Field[C, F] extends Make[C] {

  val fieldName: String = getClass.getSimpleName.replace("$", "")
  val collection: Make[C]

  def ===[C1](joined: Field[C1, F])           =   JoinOne(this, joined)
  def hashOne[C1](joined: Field[C1, F])       =   JoinOne(this, joined)
  def hashMany[C1](joined: Field[C1, F])      =   JoinMany(this, joined)

  def ===[B <: F](right: B)(implicit ev1: TypeTag[B]) =   BooleanExpression(this, right, "$eq")(ev1)
  def >(right: F)(implicit ev1: TypeTag[F])   =   BooleanExpression(this, right, "$gt")(ev1)
  def <(right: F)(implicit ev1: TypeTag[F])   =   BooleanExpression(this, right, "$lt")(ev1)
  def >=(right: F)(implicit ev1: TypeTag[F])  =   BooleanExpression(this, right, "$gte")(ev1)
  def <=(right: F)(implicit ev1: TypeTag[F])  =   BooleanExpression(this, right, "$lte")(ev1)

  override def toString = {
    collection match {
      case x: MetaTag[C] => fieldName
      case x: Field[C, _] => x+"."+fieldName
    }
  }

  def runtimeClass:Type

}

trait BaseFields {
  case class UField[C, F](override val fieldName: String, collection: Make[C]) extends Field[C, F] { override def runtimeClass: Type = ??? }
  case class StringField[C](collection: Make[C]) extends Field[C, String] {  override def runtimeClass: Type = typeOf[String]  }
  case class IntField[C](collection: Make[C]) extends Field[C, Int] {  override def runtimeClass: Type = typeOf[Int] }
  case class LongField[C](collection: Make[C]) extends Field[C, Long] {  override def runtimeClass: Type = typeOf[Long]  }
  case class InnerField[C, F](collection: Make[C])(implicit t: TypeTag[F]) extends Field[C, F] { override def runtimeClass: Type = typeOf[F] }
}

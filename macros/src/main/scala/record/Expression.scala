package record

import scala.reflect.runtime.universe._

/**
 * Created by tim on 21.05.16.
 */
trait Expression[T] {

  def &&(r: Expression[T]) =  LogicalExpression(this, r, "$and")
  def ||(r: Expression[T]) =  LogicalExpression(this, r, "$or")

}

case class BooleanExpression[C, F](left: Field[C, F], right: F, operator: String)(implicit ev1: TypeTag[F]) extends Expression[C] { def runtimeClass: Type = typeOf[F]  }
case class LogicalExpression[C](left: Expression[C], right: Expression[C], operator: String) extends Expression[C]
case class allExpression[C]() extends Expression[C]

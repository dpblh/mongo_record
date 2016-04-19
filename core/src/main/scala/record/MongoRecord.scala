package record

/**
 * Created by tim on 16.04.16.
 */
trait MongoRecord extends TypedI {

  def scheme[T] = new Scheme[T]()

  class Scheme[T]()

  def from[T1](t1: Scheme[T1])(f:T1 => FinalExpression):FinalExpression = f(null.asInstanceOf[T1])

  def where[T1](f: Expression):WhereExpression = ???


}

trait Expression
case class BooleanExpression[A](t: TypedField[A], a: A) extends Expression {
  override def toString() = t.name
}
case class LogicalExpression() extends Expression
case class WhereExpression() {
  def select[T1](t1: T1):FinalExpression = ???
}
case class FinalExpression()

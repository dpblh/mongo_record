package record

import query.Raw

/**
 * Created by tim on 16.04.16.
 */
trait TypedField[-A1] {

  val name:String

  def === (s: A1):Expression = BooleanExpression(this, s)

}

abstract class TypedFieldImpl[-A] extends TypedField[A]

trait TypedI {
  implicit def string2Typed(s: String):TypedField[String] = new TypedFieldImpl[String]{ val name = Raw.getName(s) }
  implicit def number2Typed(s: Int):TypedField[Int] = new TypedFieldImpl[Int]{ val name = Raw.getName(s) }
}

package record.signatures

import record._

import scala.reflect.runtime.universe._

/**
 * Created by tim on 21.05.16.
 */
trait FromSignatures {

  def from[T <: M, C, R](c: T)(c1: T => SelectState[R])(implicit ev1: TypeTag[C]): SelectQuery[R] = SelectQuery[R](c, c1(c), typeOf[C])

}
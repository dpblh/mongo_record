package record.signatures

import record._

/**
 * Created by tim on 21.05.16.
 */
trait ModifySignatures {

  def update[T <: M](c: T)(c1: T => ModifyState[_]) = ModifyQuery(c, c1(c))

}

trait ModifyState[C] {

  def set[F](left: Field[C, F], right: F)         = SetState(this :: updates, left, right)
  def unset(left: Field[C, _])                    = UnsetState(this :: updates, left)
  def inc(left: Field[C, Int], right: Int)        = IncState(this :: updates, left, right)
  def mul(left: Field[C, Int], right: Int)        = MulState(this :: updates, left, right)
  def rename[F](left: Field[C, F], right: String) = RenameState(this :: updates, left, right)
  def min(left: Field[C, Int], right: Int)        = MinState(this :: updates, left, right)
  def max(left: Field[C, Int], right: Int)        = MaxState(this :: updates, left, right)

  //TODO
  private[record] val updates: List[ModifyState[C]] = Nil
  private[record] val left: Field[C, _] = null
  private[record] val right: Any = null
  private[record] def condition = (this :: updates).reverse.head.asInstanceOf[WhereState[C]]
  private[record] def modify = (this :: updates).reverse.tail.groupBy(_.getClass)
}

case class SetState[C, F](override val updates: List[ModifyState[C]], override val left: Field[C, F], override val right: F) extends ModifyState[C]
case class IncState[C, F](override val updates: List[ModifyState[C]], override val left: Field[C, F], override val right: F) extends ModifyState[C]
case class MulState[C, F](override val updates: List[ModifyState[C]], override val left: Field[C, F], override val right: F) extends ModifyState[C]
case class RenameState[C, F](override val updates: List[ModifyState[C]], override val left: Field[C, F], override val right: String) extends ModifyState[C]
case class UnsetState[C, F](override val updates: List[ModifyState[C]], override val left: Field[C, F]) extends ModifyState[C] {  override val right = 1  }
case class MinState[C](override val updates: List[ModifyState[C]], override val left: Field[C, Int], override val right: Int) extends ModifyState[C]
case class MaxState[C](override val updates: List[ModifyState[C]], override val left: Field[C, Int], override val right: Int) extends ModifyState[C]

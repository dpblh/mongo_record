package record.signatures

import record._

/**
 * Created by tim on 21.05.16.
 */
trait WhereSignatures {

  def where[C](c: Expression[C])  =    WhereState(c)
  def where[C]                    =    WhereState(allExpression[C]())

}

case class WhereState[C](c: Expression[C]) extends ModifyState[C] {

  def select(c1: M)                         =      SelectEntity[C](c, c1)
  def select[F](c1: Field[C, F])            =     SelectFields1(c, c1)
  def select[F1, F2](c1: Field[C, F1],
                     c2: Field[C, F2])      =     SelectFields2(c, c1, c2)
  def select[F1, F2, F3](c1: Field[C, F1],
                         c2: Field[C, F2],
                         c3: Field[C, F3])  =     SelectFields3(c, c1, c2, c3)
  def select[F1, F2, F3, F4](c1: Field[C, F1],
                             c2: Field[C, F2],
                             c3: Field[C, F3],
                             c4: Field[C, F4])  =     SelectFields4(c, c1, c2, c3, c4)
  def select[F1, F2, F3, F4, F5](c1: Field[C, F1],
                                 c2: Field[C, F2],
                                 c3: Field[C, F3],
                                 c4: Field[C, F4],
                                 c5: Field[C, F5])  =     SelectFields5(c, c1, c2, c3, c4, c5)
  def select[F1, F2, F3, F4, F5, F6](c1: Field[C, F1],
                                 c2: Field[C, F2],
                                 c3: Field[C, F3],
                                 c4: Field[C, F4],
                                 c5: Field[C, F5],
                                 c6: Field[C, F6])  =     SelectFields6(c, c1, c2, c3, c4, c5, c6)
  def select[F1, F2, F3, F4, F5, F6, F7](c1: Field[C, F1],
                                     c2: Field[C, F2],
                                     c3: Field[C, F3],
                                     c4: Field[C, F4],
                                     c5: Field[C, F5],
                                     c6: Field[C, F6],
                                     c7: Field[C, F7])  =     SelectFields7(c, c1, c2, c3, c4, c5, c6, c7)
  def select[F1, F2, F3, F4, F5, F6, F7, F8](c1: Field[C, F1],
                                     c2: Field[C, F2],
                                     c3: Field[C, F3],
                                     c4: Field[C, F4],
                                     c5: Field[C, F5],
                                     c6: Field[C, F6],
                                     c7: Field[C, F7],
                                     c8: Field[C, F8])  =     SelectFields8(c, c1, c2, c3, c4, c5, c6, c7, c8)
  def select[F1, F2, F3, F4, F5, F6, F7, F8, F9](c1: Field[C, F1],
                                     c2: Field[C, F2],
                                     c3: Field[C, F3],
                                     c4: Field[C, F4],
                                     c5: Field[C, F5],
                                     c6: Field[C, F6],
                                     c7: Field[C, F7],
                                     c8: Field[C, F8],
                                     c9: Field[C, F9])  =     SelectFields9(c, c1, c2, c3, c4, c5, c6, c7, c8, c9)


  def on[C1, F](f: => Join[C,C1,F]) = JoinStateYield1(c, f)
  def on[C1, C2, F](f1: => Join[C,C1,F], f2: => Join[_,C2,F]) = JoinStateYield2(c, f1, f2)

}

trait SelectState[R] {  val w: Expression[_]  }

case class SelectEntity[C](w: Expression[_], c: M) extends SelectState[C]

case class SelectFields1[C, F1](w: Expression[_], c1: Field[C, F1]) extends SelectState[F1]
case class SelectFields2[C, F1, F2](w: Expression[_], c1: Field[C, F1], c2: Field[C, F2]) extends SelectState[(F1, F2)]
case class SelectFields3[C, F1, F2, F3](w: Expression[_], c1: Field[C, F1], c2: Field[C, F2], c3: Field[C, F3]) extends SelectState[(F1, F2, F3)]
case class SelectFields4[C, F1, F2, F3, F4](w: Expression[_], c1: Field[C, F1], c2: Field[C, F2], c3: Field[C, F3], c4: Field[C, F4]) extends SelectState[(F1, F2, F3, F4)]
case class SelectFields5[C, F1, F2, F3, F4, F5](w: Expression[_], c1: Field[C, F1], c2: Field[C, F2], c3: Field[C, F3], c4: Field[C, F4], c5: Field[C, F5]) extends SelectState[(F1, F2, F3, F4, F5)]
case class SelectFields6[C, F1, F2, F3, F4, F5, F6](w: Expression[_], c1: Field[C, F1], c2: Field[C, F2], c3: Field[C, F3], c4: Field[C, F4], c5: Field[C, F5], c6: Field[C, F6]) extends SelectState[(F1, F2, F3, F4, F5, F6)]
case class SelectFields7[C, F1, F2, F3, F4, F5, F6, F7](w: Expression[_], c1: Field[C, F1], c2: Field[C, F2], c3: Field[C, F3], c4: Field[C, F4], c5: Field[C, F5], c6: Field[C, F6], c7: Field[C, F7]) extends SelectState[(F1, F2, F3, F4, F5, F6, F7)]
case class SelectFields8[C, F1, F2, F3, F4, F5, F6, F7, F8](w: Expression[_], c1: Field[C, F1], c2: Field[C, F2], c3: Field[C, F3], c4: Field[C, F4], c5: Field[C, F5], c6: Field[C, F6], c7: Field[C, F7], c8: Field[C, F8]) extends SelectState[(F1, F2, F3, F4, F5, F6, F7, F8)]
case class SelectFields9[C, F1, F2, F3, F4, F5, F6, F7, F8, F9](w: Expression[_], c1: Field[C, F1], c2: Field[C, F2], c3: Field[C, F3], c4: Field[C, F4], c5: Field[C, F5], c6: Field[C, F6], c7: Field[C, F7], c8: Field[C, F8], c9: Field[C, F9]) extends SelectState[(F1, F2, F3, F4, F5, F6, F7, F8, F9)]

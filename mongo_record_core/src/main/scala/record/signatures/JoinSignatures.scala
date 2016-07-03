package record.signatures

import record._

/**
 * Created by tim on 21.05.16.
 */
trait JoinSignatures {

  def join[T <: M, T1 <: M, R](c: T, c1: T1)
                              (f: (T, T1) => JoinStateYield[R])               = JoinQuery(f(c, c1), c)
  def join[T <: M, T1 <: M, T2 <: M, R](c: T, c1: T1, c2: T2)
                                       (f: (T, T1, T2) => JoinStateYield[R])  = JoinQuery(f(c, c1, c2), c)

}

trait Join[C,C1,F] {
  val owner: Field[_, F]
  val joined: Field[_, F]
}

case class JoinOne[C, C1, F](owner: Field[C, F], joined: Field[C1, F]) extends Join[C,Option[C1],F]
case class JoinMany[C, C1, F](owner: Field[C, F], joined: Field[C1, F]) extends Join[C,List[C1],F]

trait JoinStateYield[R] { val condition: Expression[_]; val joinExpression:Seq[join] }
case class JoinStateYield1[C1, C2](condition: Expression[_], j1: Join[C1, C2, _]) extends JoinStateYield[(C1, C2)] { val joinExpression = Seq(j1) }
case class JoinStateYield2[C1, C2, C3](condition: Expression[_], j1: Join[C1, C2, _], j2: Join[_, C3, _]) extends JoinStateYield[(C1, C2, C3)] { val joinExpression = Seq(j1, j2) }

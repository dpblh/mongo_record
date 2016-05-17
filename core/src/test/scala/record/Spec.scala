package record

import org.scalatest.{Matchers, FreeSpec}

/**
 * Created by tim on 11.05.16.
 */
trait Spec extends FreeSpec with Matchers {

  implicit def query2string(q: Query):String = q.toString

  def yes(left: String, right: String):Unit = left.replaceAll("\\s", "") shouldBe right.replaceAll("\\s", "")

  def yes(left: Option[Any], right: Any):Unit = left match {
    case Some(x)  => x shouldBe right
    case None     => None shouldBe right
  }

}

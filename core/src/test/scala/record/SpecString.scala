package record

import org.scalatest.{Matchers, FreeSpec}

/**
 * Created by tim on 11.05.16.
 */
trait SpecString extends FreeSpec with Matchers {

  implicit def query2string(q: Query):String = q.toString

  def yes(left: String, right: String):Unit = left.replaceAll("\\s", "") shouldBe right.replaceAll("\\s", "")

}

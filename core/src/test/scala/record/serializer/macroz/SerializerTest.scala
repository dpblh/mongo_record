package record.serializer.macroz
import record.Spec
import record.imports._

/**
 * Created by tim on 09.05.16.
 */

case class Address(region: String, city: String)

case class Person(name: String, age: Int, address: Address)

case class Point(x: Int, y: Int)

case class Fields(f_double: Double, f_boolean: Boolean, f_int: Int, f_long: Long, f_string: String, f_float: Float, f_byte: Byte, f_big_int: BigInt, f_bid_decimal: BigDecimal, f_date: java.util.Date, f_calendar: java.util.Calendar, f_option: Option[Point], f_list: List[Point])

class SerializerTest extends Spec {

  mapper[Int].to(123) shouldBe 123
  mapper[String].to("string") shouldBe "string"
  mapper[Long].to(1l) shouldBe 1l
  mapper[Double].to(1d) shouldBe 1d
  mapper[Float].to(1f) shouldBe 1f
  mapper[Byte].to(1.toByte) shouldBe 1.toByte
  mapper[BigInt].to(BigInt(1)) shouldBe 1.toString
  mapper[BigDecimal].to(BigDecimal(1)) shouldBe 1.toString
  mapper[Boolean].to(true) shouldBe true
  mapper[Array[Byte]].to(Array(1.toByte, 2.toByte)) shouldBe Array(1.toByte, 2.toByte)
  yes(mapper[List[String]].to(Nil).toString, "[]")
  yes(mapper[List[String]].to("string" :: Nil).toString, """["string"]""")
  yes(mapper[Seq[String]].to(Nil).toString, "[]")
  yes(mapper[Set[String]].to(Set()).toString, "[]")
  yes(mapper[Map[String, String]].to(Map("string" -> "string")).toString, """{ "string" : "string" }""")

  mapper[Option[String]].to(None).asInstanceOf[AnyRef] should equal(null)
  mapper[Option[String]].to(Some("string")) shouldBe "string"

  val current_date = new java.util.Date()
  mapper[java.util.Date].to(current_date) shouldBe current_date.getTime
  val current_calendar = java.util.Calendar.getInstance()
  mapper[java.util.Calendar].to(current_calendar) shouldBe current_calendar.getTimeInMillis

  yes(mapper[Address].to(Address("Kal", "Tver")).toString, """{ "region" : "Kal", "city" : "Tver" }""")
  yes(mapper[Person].to(Person("tim", 23, Address("Kal", "Tver"))).toString, """{ "name" : "tim" , "age" : 23 , "address" : { "region" : "Kal" , "city" : "Tver"}}""")

  yes(mapper[Fields].to(
    Fields(
      1.1,
      f_boolean = true,
      1,
      1l,
      "string",
      1f,
      1.toByte,
      BigInt(1000),
      BigDecimal(100),
      current_date,
      current_calendar,
      Some(Point(0, 0)),
      Point(0, 0) :: Nil
    )
  ).toString, s"""{ "f_double" : 1.1 , "f_boolean" : true , "f_int" : 1 , "f_long" : 1 , "f_string" : "string" , "f_float" : 1.0 , "f_byte" : 1 , "f_big_int" : "1000" , "f_bid_decimal" : "100" , "f_date" : ${current_date.getTime} , "f_calendar" : ${current_calendar.getTimeInMillis} , "f_option" : { "x" : 0 , "y" : 0} , "f_list" : [ { "x" : 0 , "y" : 0}]}""")

}

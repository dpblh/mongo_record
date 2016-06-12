package record.serializer.runtime

import record.Spec
import record.runtime.serializer.DBObjectSerializer._

/**
 * Created by tim on 09.05.16.
 */

case class Address(region: String, city: String)

case class Person(name: String, age: Int, address: Address)

case class Point(x: Int, y: Int)

case class Fields(f_double: Double, f_boolean: Boolean, f_int: Int, f_long: Long, f_string: String, f_float: Float, f_byte: Byte, f_byte_array: Array[Byte], f_big_int: BigInt, f_bid_decimal: BigDecimal, f_date: java.util.Date, f_calendar: java.util.Calendar, f_option: Option[Point], f_list: List[Point])

class SerializerTest extends Spec {

  asDBObject[Int](123) shouldBe 123
  asDBObject[String]("string") shouldBe "string"
  asDBObject[Long](1l) shouldBe 1l
  asDBObject[Double](1d) shouldBe 1d
  asDBObject[Float](1f) shouldBe 1f
  asDBObject[Byte](1.toByte) shouldBe 1.toByte
  asDBObject[BigInt](BigInt(1)) shouldBe 1.toString
  asDBObject[BigDecimal](BigDecimal(1)) shouldBe 1.toString
  asDBObject[Boolean](true) shouldBe true
  asDBObject[Array[Byte]](Array(1.toByte, 2.toByte)) shouldBe Array(1.toByte, 2.toByte)
  yes(asDBObject[List[String]](Nil).toString, "[]")
  yes(asDBObject[List[String]]("string" :: Nil).toString, """["string"]""")
  yes(asDBObject[Seq[String]](Nil).toString, "[]")
  yes(asDBObject[Set[String]](Set[String]()).toString, "[]")
  yes(asDBObject[Map[String, String]](Map("string" -> "string")).toString, """{ "string" : "string" }""")

  asDBObject[Option[String]](None).asInstanceOf[AnyRef] should equal(null)
  asDBObject[Option[String]](Some("string")) shouldBe "string"

  val current_date = new java.util.Date()
  asDBObject[java.util.Date](current_date) shouldBe current_date.getTime
  val current_calendar = java.util.Calendar.getInstance()
  asDBObject[java.util.Calendar](current_calendar) shouldBe current_calendar.getTimeInMillis

  yes(asDBObject[Address](Address("Kal", "Tver")).toString, """{ "region" : "Kal", "city" : "Tver" }""")
  yes(asDBObject[Person](Person("tim", 23, Address("Kal", "Tver"))).toString, """{ "name" : "tim" , "age" : 23 , "address" : { "region" : "Kal" , "city" : "Tver"}}""")

  yes(asDBObject[Fields](
    Fields(
      1.1,
      f_boolean = true,
      1,
      1l,
      "string",
      1f,
      1.toByte,
      Array(1.toByte),
      BigInt(1000),
      BigDecimal(100),
      current_date,
      current_calendar,
      Some(Point(0, 0)),
      Point(0, 0) :: Nil
    )
  ).toString, s"""{ "f_double" : 1.1 , "f_boolean" : true , "f_int" : 1 , "f_long" : 1 , "f_string" : "string" , "f_float" : 1.0 , "f_byte" : 1 , "f_byte_array" : <Binary Data> , "f_big_int" : "1000" , "f_bid_decimal" : "100" , "f_date" : ${current_date.getTime} , "f_calendar" : ${current_calendar.getTimeInMillis} , "f_option" : { "x" : 0 , "y" : 0} , "f_list" : [ { "x" : 0 , "y" : 0}]}""")

}

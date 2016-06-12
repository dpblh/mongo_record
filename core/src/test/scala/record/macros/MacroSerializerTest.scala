package record.macros

import record.{UtilsRecord, Spec}
import record.macroses.serializer.DBObjectSerializer._

/**
 * Created by tim on 09.05.16.
 */

case class Address(region: String, city: String)

case class Person(name: String, age: Int, address: Address)

case class Point(x: Int, y: Int)

case class Fields(f_double: Double, f_boolean: Boolean, f_int: Int, f_long: Long, f_string: String, f_float: Float, f_byte: Byte, f_byte_array: Array[Byte], f_big_int: BigInt, f_bid_decimal: BigDecimal, f_date: java.util.Date, f_calendar: java.util.Calendar, f_option: Option[Point], f_list: List[Point])

class MacroSerializerTest extends Spec {

  DBOGenerator[Int].asDBObject(123) shouldBe 123
  DBOGenerator[String].asDBObject("string") shouldBe "string"
  DBOGenerator[Long].asDBObject(1l) shouldBe 1l
  DBOGenerator[Double].asDBObject(1d) shouldBe 1d
  DBOGenerator[Float].asDBObject(1f) shouldBe 1f
  DBOGenerator[Byte].asDBObject(1.toByte) shouldBe 1.toByte
  DBOGenerator[BigInt].asDBObject(BigInt(1)) shouldBe 1.toString
  DBOGenerator[BigDecimal].asDBObject(BigDecimal(1)) shouldBe 1.toString
  DBOGenerator[Boolean].asDBObject(true) shouldBe true
  DBOGenerator[Array[Byte]].asDBObject(Array(1.toByte, 2.toByte)) shouldBe Array(1.toByte, 2.toByte)
  yes(DBOGenerator[List[String]].asDBObject(Nil).toString, "[]")
  yes(DBOGenerator[List[String]].asDBObject("string" :: Nil).toString, """["string"]""")
  yes(DBOGenerator[Seq[String]].asDBObject(Nil).toString, "[]")
  yes(DBOGenerator[Set[String]].asDBObject(Set()).toString, "[]")
  yes(DBOGenerator[Map[String, String]].asDBObject(Map("string" -> "string")).toString, """{ "string" : "string" }""")

  DBOGenerator[Option[String]].asDBObject(None).asInstanceOf[AnyRef] should equal(null)
  DBOGenerator[Option[String]].asDBObject(Some("string")) shouldBe "string"

  val current_date = new java.util.Date()
  DBOGenerator[java.util.Date].asDBObject(current_date) shouldBe current_date.getTime
  val current_calendar = java.util.Calendar.getInstance()
  DBOGenerator[java.util.Calendar].asDBObject(current_calendar) shouldBe current_calendar.getTimeInMillis

  yes(DBOGenerator[Address].asDBObject(Address("Kal", "Tver")).toString, """{ "region" : "Kal", "city" : "Tver" }""")
  yes(DBOGenerator[Person].asDBObject(Person("tim", 23, Address("Kal", "Tver"))).toString, """{ "name" : "tim" , "age" : 23 , "address" : { "region" : "Kal" , "city" : "Tver"}}""")

  yes(DBOGenerator[Fields].asDBObject(
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

package record.serializer.runtime

import com.mongodb.util.JSON._
import record.Spec
import record.runtime.serializer.DBObjectSerializer._

/**
 * Created by tim on 12.06.16.
 */
case class IntroPoint(x: Point, y: Point)
class DeserializeTest extends Spec {

  fromDBObject[Int](parse("123")) shouldBe 123
  fromDBObject[String](parse( """ "string" """)) shouldBe "string"
  fromDBObject[Long](parse( """ 100000000000 """)) shouldBe 100000000000l
  fromDBObject[Double](parse( """ 100000000000.0 """)) shouldBe 100000000000.0d
  fromDBObject[Float](parse( """ 100000000000.0 """)) shouldBe 100000000000.0f
  fromDBObject[Byte](parse( """ 1 """)) shouldBe 1.toByte
  fromDBObject[BigInt](parse( """ "1" """)) shouldBe BigInt(1)
  fromDBObject[BigDecimal](parse( """ "1000.999999" """)) shouldBe BigDecimal("1000.999999")
  fromDBObject[Boolean](parse( """ true """)) shouldBe true
  fromDBObject[Array[Byte]](Array(1.toByte, 2.toByte)) shouldBe Array(1.toByte, 2.toByte)

  fromDBObject[List[String]](parse( """ [] """)) shouldBe Nil
  fromDBObject[List[String]](parse( """ ["string"] """)) shouldBe "string" :: Nil

  fromDBObject[Seq[String]](parse( """ [] """)) shouldBe Seq()
  fromDBObject[Set[String]](parse( """ [] """)) shouldBe Set()
  fromDBObject[Map[String, String]](parse( """ {"string" : "string"} """)) shouldBe Map("string" -> "string")
  fromDBObject[Map[String, Map[String, String]]](parse( """ {"string" : { "string" : "string" }} """)) shouldBe Map("string" -> Map("string" -> "string"))

  fromDBObject[Option[String]](parse( """  """)) shouldBe None
  fromDBObject[Option[String]](parse( """ "string" """)) shouldBe Some("string")

  val current_date = new java.util.Date()
  fromDBObject[java.util.Date](parse( s""" ${current_date.getTime} """)) shouldBe current_date
  val current_calendar = java.util.Calendar.getInstance()
  fromDBObject[java.util.Calendar](parse( s""" ${current_calendar.getTimeInMillis} """)) shouldBe current_calendar

  fromDBObject[Address](parse( """{ "region" : "Kal", "city" : "Tver" }""")) shouldBe Address("Kal", "Tver")
  fromDBObject[Person](parse( """{ "name" : "tim" , "age" : 23 , "address" : { "region" : "Kal" , "city" : "Tver"}}""")) shouldBe Person("tim", 23, Address("Kal", "Tver"))

  fromDBObject[Point](parse(""" { "x" : 1, "y" : 1 } """)) shouldBe Point(1, 1)
  fromDBObject[IntroPoint](parse(""" { "x" : { "x" : 1, "y" : 1 }, "y" : { "x" : 1, "y" : 1 } } """)) shouldBe IntroPoint(Point(1, 1), Point(1, 1))
  fromDBObject[List[Point]](parse(""" [{ "x" : 1, "y" : 1 }] """)) shouldBe Point(1, 1)::Nil

  fromDBObject[Fields](
    parse(s"""{ "f_double" : 1.1 , "f_boolean" : true , "f_int" : 1 , "f_long" : 1 , "f_string" : "string" , "f_float" : 1.0 , "f_byte" : 1 , "f_big_int" : "1000" , "f_bid_decimal" : "100" , "f_date" : "${current_date.getTime}" , "f_calendar" : "${current_calendar.getTimeInMillis}" , "f_option" : { "x" : 0 , "y" : 0} , "f_list" : [ { "x" : 0 , "y" : 0}]}""")
  ) shouldBe Fields(
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

}

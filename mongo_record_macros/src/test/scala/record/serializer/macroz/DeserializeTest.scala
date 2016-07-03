package record.serializer.macroz

import com.mongodb.util.JSON._
import record.Spec
import record.imports._

/**
 * Created by tim on 12.06.16.
 */
case class IntroPoint(x: Point, y: Point)
class DeserializeTest extends Spec {

  mapper[Int].from(parse("123")) shouldBe 123
  mapper[String].from(parse( """ "string" """)) shouldBe "string"
  mapper[Long].from(parse( """ 100000000000 """)) shouldBe 100000000000l
  mapper[Double].from(parse( """ 100000000000.0 """)) shouldBe 100000000000.0d
  mapper[Float].from(parse( """ 100000000000.0 """)) shouldBe 100000000000.0f
  mapper[Byte].from(parse( """ 1 """)) shouldBe 1.toByte
  mapper[BigInt].from(parse( """ "1" """)) shouldBe BigInt(1)
  mapper[BigDecimal].from(parse( """ "1000.999999" """)) shouldBe BigDecimal("1000.999999")
  mapper[Boolean].from(parse( """ true """)) shouldBe true
  mapper[Array[Byte]].from(Array(1.toByte, 2.toByte)) shouldBe Array(1.toByte, 2.toByte)

  mapper[List[String]].from(parse( """ [] """)) shouldBe Nil
  mapper[List[String]].from(parse( """ ["string"] """)) shouldBe "string" :: Nil

  mapper[Seq[String]].from(parse( """ [] """)) shouldBe Seq()
  mapper[Set[String]].from(parse( """ [] """)) shouldBe Set()
  mapper[Map[String, String]].from(parse( """ {"string" : "string"} """)) shouldBe Map("string" -> "string")
  mapper[Map[String, Map[String, String]]].from(parse( """ {"string" : { "string" : "string" }} """)) shouldBe Map("string" -> Map("string" -> "string"))

  mapper[Option[String]].from(parse( """  """)) shouldBe None
  mapper[Option[String]].from(parse( """ "string" """)) shouldBe Some("string")

  val current_date = new java.util.Date()
  mapper[java.util.Date].from(parse( s""" ${current_date.getTime} """)) shouldBe current_date
  val current_calendar = java.util.Calendar.getInstance()
  mapper[java.util.Calendar].from(parse( s""" ${current_calendar.getTimeInMillis} """)) shouldBe current_calendar

  mapper[Address].from(parse( """{ "region" : "Kal", "city" : "Tver" }""")) shouldBe Address("Kal", "Tver")
  mapper[Person].from(parse( """{ "name" : "tim" , "age" : 23 , "address" : { "region" : "Kal" , "city" : "Tver"}}""")) shouldBe Person("tim", 23, Address("Kal", "Tver"))

  mapper[Point].from(parse(""" { "x" : 1, "y" : 1 } """)) shouldBe Point(1, 1)
  mapper[IntroPoint].from(parse(""" { "x" : { "x" : 1, "y" : 1 }, "y" : { "x" : 1, "y" : 1 } } """)) shouldBe IntroPoint(Point(1, 1), Point(1, 1))
  mapper[List[Point]].from(parse(""" [{ "x" : 1, "y" : 1 }] """)) shouldBe Point(1, 1)::Nil

  mapper[Fields].from(
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

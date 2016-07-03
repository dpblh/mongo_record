package record.e2e.fields

import record.Spec
import record.imports._
import record.macroz.serializer.entityName

/**
 * Created by tim on 01.06.16.
 */
case class Point(x: Int, y: Int)
@entityName(name = "fields_new") case  class Fields(f_double: Double, f_boolean: Boolean, f_int: Int, f_long: Long, f_string: String, f_float: Float,  f_byte: Byte,  f_byte_array: Array[Byte],  f_big_int: BigInt, f_bid_decimal: BigDecimal, f_date: java.util.Date, f_calendar: java.util.Calendar, f_option: Option[Point], f_list: List[Point]) {
  def save(): Unit = Fields.insert(this).flash
}
object Fields extends SingleRecord {
  def mt = meta[Fields]
}

class FieldsTest extends Spec {

  Fields.where.remove

  Fields(1.1, true, 1, 1l, "string", 1f, 1.toByte, Array(1.toByte), BigInt(1000), BigDecimal(100), new java.util.Date(), java.util.Calendar.getInstance(), Some(Point(0,0)), Point(0,0)::Nil).save()
  Fields(1.1, true, 1, 1l, "string", 1f, 1.toByte, Array(1.toByte), BigInt(1000), BigDecimal(100), new java.util.Date(), java.util.Calendar.getInstance(), Some(Point(0,0)), Point(0,0)::Nil).save()
  Fields(1.1, true, 1, 1l, "string", 1f, 1.toByte, Array(1.toByte), BigInt(1000), BigDecimal(100), new java.util.Date(), java.util.Calendar.getInstance(), None, Nil).save()
  Fields(1.1, true, 1, 1l, "string", 1f, 1.toByte, Array(1.toByte), BigInt(1000), BigDecimal(100), new java.util.Date(), java.util.Calendar.getInstance(), None, Nil).save()

  Fields.where.count shouldBe 4


  Fields.where.fetch.length shouldBe 4

  Fields.find { field =>
    where select(field.f_double, field.f_boolean, field.f_int, field.f_long, field.f_string, field.f_float,  field.f_byte,  field.f_byte_array,  field.f_big_int, field.f_bid_decimal, field.f_date, field.f_calendar)
  }.fetch.length shouldBe 4

  Fields.find { field =>
    where select(field.f_option, field.f_list)
  }.fetch.length shouldBe 4

}

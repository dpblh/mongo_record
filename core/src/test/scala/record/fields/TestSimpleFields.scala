package record.fields

import record.imports._
import record.Spec

/**
 * Created by tim on 01.06.16.
 */
case class Pint(x: Int, y: Int)
case class Fields(f_double: Double, f_boolean: Boolean, f_int: Int, f_long: Long, f_string: String, f_float: Float,  f_byte: Byte,  f_byte_array: Array[Byte],  f_big_int: BigInt, f_bid_decimal: BigDecimal, f_date: java.util.Date, f_calendar: java.util.Calendar, f_option: Option[Pint]) {
  def save(): Unit = Fields.insert(this).flash
}
object Fields extends Meta[Fields] {

  object f_double extends DoubleField(this)
  object f_boolean extends BooleanField(this)
  object f_int extends IntField(this)
  object f_long extends LongField(this)
  object f_string extends StringField(this)
  object f_float extends FloatField(this)
  object f_byte extends ByteField(this)
  object f_byte_array extends ByteArrayField(this)
  object f_big_int extends BigIntField(this)
  object f_bid_decimal extends BigDecimalField(this)
  object f_date extends DateField(this)
  object f_calendar extends CalendarField(this)
  object f_option extends OptionField[Fields, Pint](this)

}

class TestSimpleFields extends Spec {

  Fields.where.remove

  Fields(1.1, true, 1, 1l, "string", 1f, 1.toByte, Array(1.toByte), BigInt(1000), BigDecimal(100), new java.util.Date(), java.util.Calendar.getInstance(), Some(Pint(0,0))).save()
  Fields(1.1, true, 1, 1l, "string", 1f, 1.toByte, Array(1.toByte), BigInt(1000), BigDecimal(100), new java.util.Date(), java.util.Calendar.getInstance(), Some(Pint(0,0))).save()
  Fields(1.1, true, 1, 1l, "string", 1f, 1.toByte, Array(1.toByte), BigInt(1000), BigDecimal(100), new java.util.Date(), java.util.Calendar.getInstance(), None).save()
  Fields(1.1, true, 1, 1l, "string", 1f, 1.toByte, Array(1.toByte), BigInt(1000), BigDecimal(100), new java.util.Date(), java.util.Calendar.getInstance(), None).save()

  Fields.where.count shouldBe 4


  Fields.where.fetch.length shouldBe 4

  Fields.find { field =>
    where select(field.f_double, field.f_boolean, field.f_int, field.f_long, field.f_string, field.f_float,  field.f_byte,  field.f_byte_array,  field.f_big_int, field.f_bid_decimal, field.f_date, field.f_calendar)
  }.fetch.length shouldBe 4

  Fields.find { field =>
    where select field.f_option
  }.fetch.length shouldBe 4

}

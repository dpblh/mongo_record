package record.fields

import record.imports._
import record.Spec

/**
 * Created by tim on 01.06.16.
 */
case class Fields(f_double: Double, f_boolean: Boolean, f_int: Int, f_long: Long, f_string: String, f_float: Float,  f_byte: Byte,  f_byte_array: Array[Byte],  f_big_int: BigInt) {
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

}

class TestAnyValFields extends Spec {

  Fields.where.remove

  Fields(1.1, true, 1, 1l, "string", 1f, 1.toByte, Array(1.toByte), BigInt(1000)).save()
  Fields(1.1, true, 1, 1l, "string", 1f, 1.toByte, Array(1.toByte), BigInt(1000)).save()
  Fields(1.1, true, 1, 1l, "string", 1f, 1.toByte, Array(1.toByte), BigInt(1000)).save()
  Fields(1.1, true, 1, 1l, "string", 1f, 1.toByte, Array(1.toByte), BigInt(1000)).save()

  Fields.where.count shouldBe 4

  Fields.find { field =>
    where select(field.f_double, field.f_boolean, field.f_int, field.f_long, field.f_string, field.f_float,  field.f_byte,  field.f_byte_array,  field.f_big_int)
  }.fetch.length shouldBe 4

}

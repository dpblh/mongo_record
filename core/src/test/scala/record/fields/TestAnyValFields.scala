package record.fields

import record.imports._
import record.Spec

/**
 * Created by tim on 01.06.16.
 */
case class Fields(f_double: Double, f_boolean: Boolean, f_int: Int, f_long: Long, f_string: String) {
  def save(): Unit = Fields.insert(this).flash
}
object Fields extends Meta[Fields] {

  object f_double extends DoubleField(this)
  object f_boolean extends BooleanField(this)
  object f_int extends IntField(this)
  object f_long extends LongField(this)
  object f_string extends StringField(this)

}

class TestAnyValFields extends Spec {

  Fields.where.remove

  Fields(1.1, true, 1, 1l, "string").save()
  Fields(1.1, true, 1, 1l, "string").save()
  Fields(1.1, true, 1, 1l, "string").save()
  Fields(1.1, true, 1, 1l, "string").save()

  Fields.where.count shouldBe 4

  Fields.find { field =>
    where select  (field.f_double, field.f_boolean, field.f_int, field.f_long, field.f_string)
  }.fetch.length shouldBe 4

}

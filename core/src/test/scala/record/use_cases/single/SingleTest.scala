package record.use_cases.single

import record.Spec
import record.imports._
/**
 * Created by tim on 22.04.16.
 */
case class Person(id: String, name: String, fio: String, age: Int)

object Person extends Meta[Person] {

  override val collection_name: String = "person"
  override def isValid(c: Person): Boolean = c.name.nonEmpty && c.age > 18
  object id extends StringField(this)
  object name extends StringField(this)
  object age extends IntField(this)

  def findThis = find { s =>
    where(s.age > 23 && s.age < 12) select s
  }

}

class SingleTest extends Spec {

  yes(Person.findThis, """db.person.find({ "$and" : [{"age": { "$gt": 23 }}, {"age": { "$lt": 12 }}]})""")

  yes(Person.find { s =>
    where(s.age > 23 && s.age < 12) select s
  }, """db.person.find({ "$and" : [{"age": { "$gt": 23 }}, {"age": { "$lt": 12 }}]})""")

  yes(Person.find { p =>
    where(p.name === "tim") select p.name
  }, """db.person.find({"name" : "tim"}, {"name" : 1})""")

  Person.isValid(Person("", "", "", 17)) shouldBe false
  Person.isValid(Person("", "tim", "", 19)) shouldBe true

}

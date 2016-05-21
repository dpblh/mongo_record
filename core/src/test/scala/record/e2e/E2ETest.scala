package record.e2e

import org.scalatest.{FreeSpec, Matchers}
import record.MongoRecordImpl._
import record.Spec
import record.imports._

/**
 * Created by tim on 09.05.16.
 */

case class Address(region: String, city: String)
case class Person(name: String, age: Int, address: Address) {
  def save = Person.insert(this).flash
}

object Person extends Meta[Person] {

  override val collection_name: String = "person"
  object name extends StringField("name", this)
  object age extends IntField("age", this)
  object address extends InnerField[Person, Address]("address", this)

}

class E2ETest extends Spec {

  Person.where.remove

  Person("tim", 23, Address("Kal", "Tver")).save
  Person("tim", 24, Address("Kal", "Tver")).save
  Person("tim", 27, Address("Kal", "Tver")).save
  Person("klava", 28, Address("Kal", "Tver")).save
  Person.insert(Person("tim", 25, Address("Kalinin", "Tver"))).flash

  Person.find { p =>
    where(p.name === "tim") select p
  }.fetch.length shouldBe 4

  Person.find { p =>
    where(p.name === "tim") select p
  }.fetch.foreach { _.name }

  Person.find { p =>
    where(p.name === "tim") select(p.name, p.address)
  }.fetch.foreach { _._2 }

  //select field
  Person.find { p =>
    where(p.name === "tim") select p.name
  }.fetch.length shouldBe 4

  //inner type
  Person.find { p =>
    where(p.name === "tim") select p.address
  }.fetch.length shouldBe 4

  //select fields
  Person.find { p =>
    where(p.name === "tim") select(p.name, p.address)
  }.fetch.length shouldBe 4


  //Some
  yes(Person.find { p =>
    where(p.name === "klava") select p
  }.fetchOne, Person("klava", 28, Address("Kal", "Tver")))

  //None
  yes(Person.find { p =>
    where(p.name === "tim2" && p.age > 4) select p
  }.fetchOne, None)

  Person.find { p =>
    where(p.address === Address("Kalinin", "Tver")) select p
  }.fetch.length shouldBe 1

  Person.find { p =>
    where(p.name === "tim" && p.age <= 25) select p
  }.fetch.length shouldBe 3

  //update all
  Person.modify { p =>
    where(p.name === "tim") set(p.age, 29)
  }.modify()

  Person.find { p =>
    where(p.name === "tim" && p.age <= 25) select p
  }.fetch.length shouldBe 0

  yes(Person.find { p =>
    where(p.name === "klava" && p.age === 28) select p
  }.fetchOne, Person("klava", 28, Address("Kal", "Tver")))

  //update one
  Person.modify { p =>
    where(p.name === "klava") set(p.age, 29)
  }.modifyOne()

  yes(Person.find { p =>
    where(p.name === "klava" && p.age === 28) select p
  }.fetchOne, None)

  Person.where { p =>
    p.name === "tim"
  }.count shouldBe 4

  Person.where { p =>
    p.name === "tim"
  }.remove

  Person.where.count shouldBe 1


}

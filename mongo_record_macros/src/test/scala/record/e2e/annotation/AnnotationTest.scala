package record.e2e.annotation

import record.Spec
import record.imports._
import record.macroz.serializer.{entityName, mongoRecord}

/**
 * Created by tim on 25.06.16.
 */

@mongoRecord case class Address(region: String, city: String)
@mongoRecord @entityName(name = "person_no_conflict2") case class Person(name: String, @entityName(name = "old") age: Int, address: Address)

class AnnotateTest extends Spec {

  Person.where.remove

  Person("tim", 23, Address("Kal", "Tver")).save.flash
  Person("tim", 24, Address("Kal", "Tver")).save.flash
  Person("tim", 27, Address("Kal", "Tver")).save.flash
  Person("klava", 28, Address("Kal", "Tver")).save.flash
  Person("tim", 25, Address("Kalinin", "Tver")).save.flash

  Person.find { p =>
    where(p.dynamic("name") === "tim") select p
  }.fetch.length shouldBe 4

  Person.find { p =>
    where(p.dynamic("address.city") === "Tver") select p
  }.fetch.length shouldBe 5

  Person.find { p =>
    where(p.dynamic("address") === Address("Kalinin", "Tver")) select p
  }.fetch.length shouldBe 1

  Person.find { p =>
    where(p.dynamic("address") === Map("region" -> "Kalinin", "city" -> "Tver")) select p
  }.fetch.length shouldBe 1

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

  Person.find { p =>
    where(p.name === "tim") select(p.name, p.age, p.address)
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
    where(p.address.region === "Kalinin") select p
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

package record.e2e

import org.scalatest.{FreeSpec, Matchers}
import record.MongoRecordImpl._
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

  def modify = update(this) _

  def predicate = condition(this) _

}

class E2ETest extends FreeSpec with Matchers {

  Person.predicate { p =>
    where(p.name === "tim")
  }.remove

  Person("tim", 1, Address("Kal", "Tver")).save
  Person("tim", 2, Address("Kal", "Tver")).save
  Person("tim", 3, Address("Kal", "Tver")).save
  Person.insert(Person("tim", 5, Address("Kalinin", "Tver"))).flash

  Person { p =>
    where(p.name === "tim") select p
  }.fetch.foreach(println)

  Person { p =>
    where(p.name === "tim") select p.name
  }.fetch.foreach(println)

  Person { p =>
    where(p.name === "tim") select p.address
  }.fetch.foreach(println)

  //Some
  Person { p =>
    where(p.name === "tim") select p
  }.fetchOne.foreach(println)

  //None
  Person { p =>
    where(p.name === "tim2" && p.age > 4) select p
  }.fetchOne.foreach(println)

  Person { p =>
    where(p.address === Address("Kalinin", "Tver")) select p
  }.fetch.foreach(println)

  //update all
  Person.modify { p =>
    where(p.name === "tim") set(p.age, 4)
  }.modify()

  //update one
  Person.modify { p =>
    where(p.name === "tim") set(p.age, 5)
  }.modifyOne()

  Person.predicate { p =>
    where(p.name === "tim")
  }.count


}

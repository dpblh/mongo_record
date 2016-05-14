package record.e2e

import org.scalatest.{FreeSpec, Matchers}
import record.MongoRecord
import record.imports._

/**
 * Created by tim on 09.05.16.
 */

case class Person(name: String, age: Int)

class E2ETest extends FreeSpec with Matchers with MongoRecord {

  object Person extends Meta[Person] {

    override val collection_name: String = "person"
    object name extends StringField("name", this)
    object age extends IntField("age", this)

    def modify = update(this) _

  }

  Person { p =>
    where(p.name === "tim") select p
  }.fetch.foreach(println)

  //Some
  Person { p =>
    where(p.name === "tim") select p
  }.fetchOne.foreach(println)

  //None
  Person { p =>
    where(p.name === "tim2") select p
  }.fetchOne.foreach(println)

  //update all
  Person.modify { p =>
    where(p.name === "tim") set(p.age, 4)
  }.modify()

  //update one
  Person.modify { p =>
    where(p.name === "tim") set(p.age, 5)
  }.modifyOne()


}

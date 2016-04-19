package query

import org.scalatest.{Matchers, FreeSpec}
import query.MongoDSL2._
/**
 * Created by tim on 18.04.16.
 */

case class Person(name: String, fio: String, age: Int)

case class Token(id: String)

object Person extends MongoDSL {
//  val person = meta[Person]

  val person = new Make[Person] {
    val collection_name = "person"
    val name = Field[Person, String]("name")
    val age = Field[Person, Int]("age")
  }

  val token = new Make[Token] {
    val collection_name = "token"
    val name = Field[Token, String]("name")
    val age = Field[Token, Int]("age")
  }

  val findAnd = from(person) { s =>
    where(s.age > 23 && s.age < 12) select s
  }

  val findOr = from(person) { s =>
    where(s.name === "tim" || s.age > 23) select s
  }

  val findAndOrPriority = from(person) { s =>
    where((s.name === "tim" || s.age > 23) && (s.name === "jon" || s.age === 21)) select s
  }

  val find = from(person) { s =>
    where(s.name === "tim" || s.age > 23 && s.age === 12) select s
  }

  val updated = update(person) { s =>
    where(s.age === 23) set(s.name := "ivan", s.age := 22)
  }



}

class MongoDSLTest extends FreeSpec with Matchers {

  Person.findAnd.toString.replaceAll("\\s", "") shouldBe "db.person.find({ $and : [{age: { $gt: '23' }}, {age: { $lt: '12' }}]})".replaceAll("\\s", "")

  Person.findOr.toString.replaceAll("\\s", "") shouldBe "db.person.find({ $or: [{name: 'tim'}, {age: { $gt: '23' }}] })".replaceAll("\\s", "")

  Person.findAndOrPriority.toString.replaceAll("\\s", "") shouldBe "db.person.find({ $and: [{ $or: [{name: 'tim'}, {age: { $gt: '23' }}]}, { $or: [{name: 'jon'}, {age: '21'}] }] })".replaceAll("\\s", "")

  Person.find.toString.replaceAll("\\s", "") shouldBe "db.person.find({ $or : [{name: 'tim'}, { $and : [{age: { $gt : '23' }}, {age : '12'}]}]})".replaceAll("\\s", "")

  Person.person.insert(Person("tim", "bay", 23)).replaceAll("\\s", "") shouldBe """db.person.insert({"name": "tim", "fio": "bay", "age": 23})""".replaceAll("\\s", "")

  Person.updated.toString.replaceAll("\\s", "") shouldBe "db.TODO.update({age : '23'}, {name : 'ivan', age : '22'})".replaceAll("\\s", "")

}

package record

import MongoRecord._

import scala.language.postfixOps

/**
 * Created by tim on 19.04.16.
 */
case class Person(id: String, name: String, fio: String, age: Int)
case class Token(person_id: String)
case class Friend(person_id: String)

object Person extends MongoRecordImpl {
//    val person = meta[Person]
//    val token = meta[Token]
//    val friend = meta[Friend]

  val person = new Make[Person] {
    val collection_name = "person"
    val id = Field[Person, String]("id", collection_name)
    val name = Field[Person, String]("name", collection_name)
    val age = Field[Person, Int]("age", collection_name)
  }

  val token = new Make[Token] {
    val collection_name = "token"
    val person_id = Field[Token, String]("person_id", collection_name)
  }

  val friend = new Make[Friend] {
    val collection_name = "friend"
    val person_id = Field[Friend, String]("person_id", collection_name)
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

  val selectFields = from(person) { s =>
    where(s.name === "tim") select (s.name, s.age)
  }

  val updated = update(person) { s =>
    where(s.age === 23) set(s.name := "ivan", s.age := 22)
  }

  val totalAge = mapReduce(person) { s =>
    where(s.name === "tim") emit(s.name, s.age) sum
  }

  val maxAge = mapReduce(person) { s =>
    where(s.name === "tim") emit(s.name, s.age) max
  }

  val joined = join(person, token) { (p, t) =>
    where(p.name === "tim") on(p.id === t.person_id)
  }

  val joinedThree = join(person, token, friend) { (p, t, f) =>
    where(p.name === "tim") on(p.id === t.person_id) on (p.id === f.person_id)
  }

}
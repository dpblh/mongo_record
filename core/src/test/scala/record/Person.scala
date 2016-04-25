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

  object token extends Make[Token] {
    override val collection_name: String = "token"
    object person_id extends Field[Token, String]("person_id", this)
  }

  object person extends Make[Person] {
    override val collection_name: String = "person"
    object id extends Field[Person, String]("id", this)
    object name extends Field[Person, String]("name", this)
    object age extends Field[Person, Int]("age", this)
  }

  object friend extends Make[Friend] {
    override val collection_name: String = "friend"
    object person_id extends Field[Friend, String]("person_id", this)
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
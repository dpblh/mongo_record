package record

import MongoRecord._
import scala.language.postfixOps

/**
 * Created by tim on 19.04.16.
 */
case class Person(id: String, name: String, fio: String, age: Int)
case class Token(person_id: String)
case class PersonFriend(person_id: String)

object Person extends MongoRecordImpl {
//    val person = meta[Person]
//    val token = meta[Token]
//    val friend = meta[PersonFriend]

  object token extends Meta[Token] {
    override val collection_name: String = "token"
    object person_id extends StringField("person_id", this)
  }

  object person extends Meta[Person] {
    override val collection_name: String = "person"
    object id extends StringField("id", this)
    object name extends StringField("name", this)
    object age extends IntField("age", this)
  }

  object friend extends Meta[PersonFriend] {
    override val collection_name: String = "person_friend"
    object person_id extends StringField("person_id", this)
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
    where(s.age === 23) set(s.name, "ivan") set(s.age, 22)
  }

  val updatedMix = update(person) { s =>
    where(s.age === 23)
      .set(s.name, "ivan")
      .set(s.age, 13)
      .inc(s.age, 2)
      .mul(s.age, 3)
      .rename(s.name, "lastName")
      .unset(s.name)
      .min(s.age, 12)
      .max(s.age, 3)
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
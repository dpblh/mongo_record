package record

/**
 * Created by tim on 19.04.16.
 */
case class Person(name: String, fio: String, age: Int)

object Person extends MongoRecordImpl {
  //  val person = meta[Person]

  val person = new Make[Person] {
    val collection_name = "person"
    val name = Field[Person, String]("name")
    val age = Field[Person, Int]("age")
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
package query

/**
 * Created by tim on 05.04.16.
 */
object Tt {

  def main(args: Array[String]): Unit = {

//    Raw.raw("qwe")
//    println(JsonMapper.mapper[Person].toJson(Person("tim", "bay", 23)))

//    SimpleStrict.strict.x
//    val tup = SimpleStrict.strictType[Person]
//
//    assert(tup.name == Field[Person, String]("name"))
//    assert(tup.age == Field[Person, Int]("age"))

//    println(Person.find)

  }


}


case class Person(name: String, fio: String, age: Int)
case class Token(id: String)

object Person extends MongoDSL { //val person = meta[Person]

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

//  from(person){ s =>
//    where(s.name === "tim" and s.age === 23 or s.age === 12) select s
//  }

  def find:String = {
    from(person){ s =>
      where(s.name === "tim" || s.age > 23 && s.age === 12) select s
    }.toString
  }

}

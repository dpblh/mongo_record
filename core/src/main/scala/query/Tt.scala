package query
import MongoDSL._

/**
 * Created by tim on 05.04.16.
 */
object Tt {

  def main(args: Array[String]): Unit = {

//    Raw.raw("qwe")
//    Raw.rawType[Person]

//    SimpleStrict.strict.x
//    val tup = SimpleStrict.strictType[Person]
//
//    assert(tup.name == Field[Person, String]("name"))
//    assert(tup.age == Field[Person, Int]("age"))

//    println(Person.person.name)

  }


}


case class Person(name: String, fio: String, age: Int, asd: Int, current_at: Long, token: Token)
case class Token(id: String)

object Person extends MongoDSL { val person = meta[Person]

//  val person = new Make[Person] {
//    val name = Field[Person, String]("name")
//    val age = Field[Person, Int]("age")
//  }
//
//  val token = new Make[Token] {
//    val name = Field[Token, String]("name")
//    val age = Field[Token, Int]("age")
//  }

  val s = from(person){ s =>
    where(s.name > "tim" and s.age > 23 and s.name === "tim" or s.age < 12) select s
  }

  println(s)

}

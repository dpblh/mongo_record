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

    println(Person.person.name)

  }


}


case class Person(name: String, age: Int)

object Person extends MongoDSL[Person] { val person = meta[Person]
  where(person.name gt "tim")
}

package record.use_cases.group

import org.scalatest.{Matchers, FreeSpec}
import record.MongoRecordImpl
import record._

/**
 * Created by tim on 22.04.16.
 */

case class Person(id: String, name: String, fio: String, age: Int)
case class Token(person_id: String)

object PersonToken extends MongoRecordImpl {

  object Token extends Make[Token] {
    override val collection_name: String = "token"
    object person_id extends Field[Token, String]("person_id", this)
  }

  object PersonService extends Make[Person] {
    override val collection_name: String = "person"
    object id extends Field[Person, String]("id", this)
    object name extends Field[Person, String]("name", this)
    object age extends Field[Person, Int]("age", this)
    
    val findAnd = from(this) { s =>
      where(s.age > 23 && s.age < 12) select s
    }
    
  }

  val joined = join(PersonService, Token) { (p, t) =>
    where(p.name === "tim") on(p.id === t.person_id)
  }

}


class GroupTest extends FreeSpec with Matchers {
  
  import PersonToken._

  PersonToken.joined.toString.replaceAll("\\s", "") shouldBe
    """
      |db.person.aggregate([{
      | $lookup: {
      |   from: "token",
      |   localField: "id",
      |   foreignField: "person_id",
      |   as: "copies_sold"
      | }
      |}])
    """.stripMargin.replaceAll("\\s", "")

  PersonService.findAnd.toString.replaceAll("\\s", "") shouldBe "db.person.find({ $and : [{age: { $gt: '23' }}, {age: { $lt: '12' }}]})".replaceAll("\\s", "")

}

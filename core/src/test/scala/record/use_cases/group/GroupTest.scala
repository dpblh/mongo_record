package record.use_cases.group

import record._

/**
 * Created by tim on 22.04.16.
 */

case class Person(id: String, name: String, fio: String, age: Int)
case class Token(person_id: String)

object PersonToken extends MongoRecord {

  object Token extends Meta[Token] {
    object person_id extends StringField(this)
  }

  object PersonService extends Meta[Person] {
    override val collection_name: String = "person"
    object id extends StringField(this)
    object name extends StringField(this)
    object age extends IntField(this)
  }

  val joined = join(PersonService, Token) { (p, t) =>
    where(p.name === "tim") on(p.id === t.person_id)
  }

}


class GroupTest extends Spec {
  
  import PersonToken._

  yes(PersonToken.joined,
    """
      |db.person.aggregate([{
      | "$lookup": {
      |   "from": "token",
      |   "localField": "id",
      |   "foreignField": "person_id",
      |   "as": "person_token__person_id"
      | }
      |}])
    """.stripMargin)

  yes(PersonService.find { s =>
    where(s.age > 23 && s.age < 12) select s
  }, """db.person.find({ "$and" : [{"age": { "$gt": 23 }}, {"age": { "$lt": 12 }}]})""")

}

package record.use_cases.group

import record._

/**
 * Created by tim on 22.04.16.
 */

case class Person(id: String, name: String, fio: String, age: Int)
case class Token(person_id: String)

object PersonToken extends MongoRecord {

  object Token extends MetaTag[Token] {
    override val collection_name: String = "token"
    object person_id extends StringField("person_id", this)
  }

  object PersonService extends MetaTag[Person] {
    override val collection_name: String = "person"
    object id extends StringField("id", this)
    object name extends StringField("name", this)
    object age extends IntField("age", this)
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
      |   "as": "copies_sold"
      | }
      |}])
    """.stripMargin)

  yes(PersonService.find { s =>
    where(s.age > 23 && s.age < 12) select s
  }, """db.person.find({ "$and" : [{"age": { "$gt": 23 }}, {"age": { "$lt": 12 }}]})""")

}

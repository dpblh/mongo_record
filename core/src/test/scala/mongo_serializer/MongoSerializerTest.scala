package mongo_serializer

import java.util.Date

import com.mongodb.{BasicDBList, BasicDBObjectBuilder}
import record.Spec
import record.serializer.DBObjectSerializer

/**
 * Created by tim on 14.05.16.
 */

case class Person(id: String, name: String, age: Int, created_at: Date)
case class Fr(name: String, person: Person, created_at: Date, friends: List[Person])

class MongoSerializerTest extends Spec {

  val create_at = new Date().getTime

  val dbPerson = BasicDBObjectBuilder.start()
    .append("id", "_id")
    .append("name", "tim")
    .append("age", 1)
    .append("created_at", BigDecimal(create_at)).get()
  val person = Person("_id", "tim", 1, new Date(create_at))

  val dbFriends = new BasicDBList()
  dbFriends.add(dbPerson)
  dbFriends.add(dbPerson)
  val friends = person::person::Nil

  val dbFr = BasicDBObjectBuilder.start()
    .append("name", "klav")
    .append("person", dbPerson)
    .append("created_at", BigDecimal(create_at))
    .append("friends", dbFriends).get()
  val fr = Fr("klav", person, new Date(create_at), friends)

  DBObjectSerializer.fromDBObject[Person](dbPerson) shouldBe person
  DBObjectSerializer.fromDBObject[Fr](dbFr) shouldBe fr


}

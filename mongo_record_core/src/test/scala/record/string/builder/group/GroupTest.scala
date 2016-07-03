package record.string.builder.group

import record._

/**
 * Created by tim on 22.04.16.
 */
case class Person(id: String, name: String, fio: String, age: Int)
case class Token(person_id: String)
case class PersonFriend(person_id: String)

object Person extends MongoRecord {

  object token extends Meta[Token] {
    object person_id extends StringField(this)
  }

  object person extends Meta[Person] {
    object id extends StringField(this)
    object name extends StringField(this)
    object fio extends StringField(this)
    object age extends IntField(this)
  }

  object friend extends Meta[PersonFriend] {
    override val entityName: String = "person_friend"
    object person_id extends StringField(this)
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

  val joined = join(person, token) { (p, t) =>
    where(p.name === "tim") on(p.id === t.person_id)
  }

  val joinedThree = join(person, token, friend) { (p, t, f) =>
    where(p.name === "tim") on(p.id === t.person_id, p.id === f.person_id)
  }

}


class GroupTest extends Spec {

  yes(Person.findAnd, """db.person.find({ "$and" : [{"age": { "$gt": 23 }}, {"age": { "$lt": 12 }}]})""")

  yes(Person.findOr, """db.person.find({ "$or": [{"name": "tim"}, {"age": { "$gt": 23 }}] })""")

  yes(Person.findAndOrPriority, """db.person.find({ "$and": [{ "$or": [{"name": "tim"}, {"age": { "$gt": 23 }}]}, { "$or": [{"name": "jon"}, {"age": 21}] }] })""")

  yes(Person.find, """db.person.find({ "$or" : [{"name": "tim"}, { "$and" : [{"age": { "$gt" : 23 }}, {"age" : 12}]}]})""")

  yes(Person.person.insert(Person("id", "tim", "bay", 23)), """db.person.insert({"id": "id", "name": "tim", "fio": "bay", "age": 23})""")

  yes(Person.updated, """db.person.update({"age" : 23}, {"$set": {"age" : 22, "name" : "ivan"}})""")

  //  yes(Person.updatedMix, """db.person.update({ "age" : 23}, { "$rename" : { "name" : "lastName"} , "$mul" : { "age" : 3} , "$inc" : { "age" : 2} , "$min" : { "age" : 12} , "$max" : { "age" : 3} , "$set" : { "age" : 13 , "name" : "ivan"} , "$unset" : { "name" : 1}})""")

  yes(Person.selectFields, """db.person.find({"name": "tim"}, {"name": 1, "age": 1})""")

  yes(Person.joined,
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

  yes(Person.joinedThree,
    """
      |db.person.aggregate([
      |{
      | "$lookup": {
      |   "from": "person_friend",
      |   "localField": "id",
      |   "foreignField": "person_id",
      |   "as": "person_person_friend__person_id"
      | }
      |},
      |{
      | "$lookup": {
      |   "from": "token",
      |   "localField": "id",
      |   "foreignField": "person_id",
      |   "as": "person_token__person_id"
      | }
      |}])
    """.stripMargin)

}

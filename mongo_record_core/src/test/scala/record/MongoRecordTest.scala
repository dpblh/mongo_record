package record

/**
 * Created by tim on 18.04.16.
 */
class MongoRecordTest extends Spec {

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

package record

import org.scalatest.{Matchers, FreeSpec}
/**
 * Created by tim on 18.04.16.
 */
class MongoRecordTest extends FreeSpec with Matchers {

  Person.findAnd.toString.replaceAll("\\s", "") shouldBe """db.person.find({ "$and" : [{"age": { "$gt": 23 }}, {"age": { "$lt": 12 }}]})""".replaceAll("\\s", "")

  Person.findOr.toString.replaceAll("\\s", "") shouldBe """db.person.find({ "$or": [{"name": "tim"}, {"age": { "$gt": 23 }}] })""".replaceAll("\\s", "")

  Person.findAndOrPriority.toString.replaceAll("\\s", "") shouldBe """db.person.find({ "$and": [{ "$or": [{"name": "tim"}, {"age": { "$gt": 23 }}]}, { "$or": [{"name": "jon"}, {"age": 21}] }] })""".replaceAll("\\s", "")

  Person.find.toString.replaceAll("\\s", "") shouldBe """db.person.find({ "$or" : [{"name": "tim"}, { "$and" : [{"age": { "$gt" : 23 }}, {"age" : 12}]}]})""".replaceAll("\\s", "")

  Person.person.insert(Person("id", "tim", "bay", 23)).toString.replaceAll("\\s", "") shouldBe """db.person.insert({'name': 'tim', 'fio': 'bay', 'age': 23})""".replaceAll("\\s", "")

  Person.updated.toString.replaceAll("\\s", "") shouldBe """db.person.update({"age" : 23}, {"$set": {"age" : 22, "name" : "ivan"}})""".replaceAll("\\s", "")

  Person.updatedMix.toString.replaceAll("\\s", "") shouldBe """db.person.update({ "age" : 23}, { "$unset" : { "name" : 1} , "$set" : { "age" : 13 , "name" : "ivan"} , "$max" : { "age" : 3} , "$min" : { "age" : 12} , "$rename" : { "name" : "lastName"} , "$mul" : { "age" : 3} , "$inc" : { "age" : 2}})""".replaceAll("\\s", "")

  Person.selectFields.toString.replaceAll("\\s", "") shouldBe """db.person.find({"name": "tim"}, {"name": 1, "age": 1})""".replaceAll("\\s", "")

  Person.joined.toString.replaceAll("\\s", "") shouldBe
    """
      |db.person.aggregate([{
      | "$lookup": {
      |   "from": "token",
      |   "localField": "id",
      |   "foreignField": "person_id",
      |   "as": "copies_sold"
      | }
      |}])
    """.stripMargin.replaceAll("\\s", "")

  Person.joinedThree.toString.replaceAll("\\s", "") shouldBe
    """
      |db.person.aggregate([
      |{
      | "$lookup": {
      |   "from": "person_friend",
      |   "localField": "id",
      |   "foreignField": "person_id",
      |   "as": "copies_sold"
      | }
      |},
      |{
      | "$lookup": {
      |   "from": "token",
      |   "localField": "id",
      |   "foreignField": "person_id",
      |   "as": "copies_sold"
      | }
      |}])
    """.stripMargin.replaceAll("\\s", "")

}

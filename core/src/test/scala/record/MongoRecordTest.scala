package record

import org.scalatest.{Matchers, FreeSpec}
/**
 * Created by tim on 18.04.16.
 */
class MongoRecordTest extends FreeSpec with Matchers {

  Person.findAnd.toString.replaceAll("\\s", "") shouldBe "db.person.find({ $and : [{age: { $gt: '23' }}, {age: { $lt: '12' }}]})".replaceAll("\\s", "")

  Person.findOr.toString.replaceAll("\\s", "") shouldBe "db.person.find({ $or: [{name: 'tim'}, {age: { $gt: '23' }}] })".replaceAll("\\s", "")

  Person.findAndOrPriority.toString.replaceAll("\\s", "") shouldBe "db.person.find({ $and: [{ $or: [{name: 'tim'}, {age: { $gt: '23' }}]}, { $or: [{name: 'jon'}, {age: '21'}] }] })".replaceAll("\\s", "")

  Person.find.toString.replaceAll("\\s", "") shouldBe "db.person.find({ $or : [{name: 'tim'}, { $and : [{age: { $gt : '23' }}, {age : '12'}]}]})".replaceAll("\\s", "")

  Person.person.insert(Person("tim", "bay", 23)).replaceAll("\\s", "") shouldBe """db.person.insert({'name': 'tim', 'fio': 'bay', 'age': 23})""".replaceAll("\\s", "")

  Person.updated.toString.replaceAll("\\s", "") shouldBe "db.TODO.update({age : '23'}, {name : 'ivan', age : '22'})".replaceAll("\\s", "")

}

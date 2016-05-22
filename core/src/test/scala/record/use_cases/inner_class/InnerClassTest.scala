package record.use_cases.inner_class

import record._
/**
 * Created by tim on 25.04.16.
 */

case class Person(id: String, email: String, personData: PersonData)
case class PersonData(lastName: String, firstName: String)
case class Address(street: String)

class InnerClassTest extends Spec with MongoRecord {

  object person extends MetaTag[Person] { self =>
    override val collection_name: String = "person"
    object id extends StringField(this)
    object email extends StringField(this)

    object personData extends InnerField[Person, PersonData](this) {
      object firstName extends StringField(this)
      object lastName extends StringField(this)

      object address extends InnerField[Person, Address](this) {
        object street extends StringField(this)
      }
    }

  }


  yes(from(person) { p =>
    where(
        p.email === "bajurovt@gmail.com" &&
        p.personData.firstName === "tim" &&
        p.personData.address.street === "Tver"
    ) select p
  }, """db.person.find({ "$and" : [ { "$and" : [ { "email" : "bajurovt@gmail.com"} , { "person_data.firstName" : "tim"}]} , { "person_data.address.street" : "Tver"}]})""")


  yes(from(person) { p =>
    where(
      p.email === "bajurovt@gmail.com" &&
        p.personData.firstName === "tim" &&
        p.personData.address.street === "Tver" &&
        p.personData.address === Address("Tver")
    ) select p}, """db.person.find({ "$and" : [ { "$and" : [ { "$and" : [ { "email" : "bajurovt@gmail.com"} , { "person_data.firstName" : "tim"}]} , { "person_data.address.street" : "Tver"}]} , { "person_data.address" : { "street" : "Tver"}}]})""")


}

package record.use_cases.inner_class

import record.imports._
import record._
/**
 * Created by tim on 25.04.16.
 */

case class Person(id: String, email: String, personData: PersonData)
case class PersonData(lastName: String, firstName: String)
case class Address(street: String)

object Person extends Meta[Person] {
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

class InnerClassTest extends Spec {

  yes(from(Person) { p =>
    where(
        p.email === "bajurovt@gmail.com" &&
        p.personData.firstName === "tim" &&
        p.personData.address.street === "Tver"
    ) select p
  }, """db.person.find({ "$and" : [ { "$and" : [ { "email" : "bajurovt@gmail.com"} , { "person_data.first_name" : "tim"}]} , { "person_data.address.street" : "Tver"}]})""")


  yes(from(Person) { p =>
    where(
      p.email === "bajurovt@gmail.com" &&
        p.personData.firstName === "tim" &&
        p.personData.address.street === "Tver" &&
        p.personData.address === Address("Tver")
    ) select p}, """db.person.find({ "$and" : [ { "$and" : [ { "$and" : [ { "email" : "bajurovt@gmail.com"} , { "person_data.first_name" : "tim"}]} , { "person_data.address.street" : "Tver"}]} , { "person_data.address" : { "street" : "Tver"}}]})""")


}

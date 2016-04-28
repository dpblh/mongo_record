package record.use_cases.inner_class

import org.scalatest.{Matchers, FreeSpec}
import record._
/**
 * Created by tim on 25.04.16.
 */

case class Person(id: String, email: String, personData: PersonData)
case class PersonData(lastName: String, firstName: String)
case class Address(street: String)

class InnerClassTest extends FreeSpec with Matchers with MongoRecordImpl {

  object person extends Make[Person] { self =>
    override val collection_name: String = "person"
    object id extends StringField("id", this)
    object email extends StringField("email", this)

    object personData extends UField[Person, PersonData]("person_data", this) {
      object firstName extends StringField("firstName", this)
      object lastName extends StringField("lastName", this)

      object address extends UField[Person, Address]("address", this) {
        object street extends StringField("street", this)
      }
    }

  }

//  Person("", "", PersonData("")).copy()


  println(from(person) { p =>
    where(
        p.email === "bajurovt@gmail.com" &&
        p.personData.firstName === "tim" &&
        p.personData.address.street === "Tver" &&
        p.personData.address === Address("Tver")
    ) select p
  }.toString.replaceAll("\\s", ""))// shouldBe "db.person.find({ $and: [{email: 'bajurovt@gmail.com'}, 'person_data.firstName': 'tim'] })".replaceAll("\\s", "")



}

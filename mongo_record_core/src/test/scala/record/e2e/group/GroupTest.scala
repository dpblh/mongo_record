package record.e2e.group

import record.imports._
import record.{MongoRecord, Spec}
/**
 * Created by tim on 17.05.16.
 */
case class Patient(id: String, snils: String, address_id: String)
case class Address(id: String, street: String)
case class PersonalData(person_id: String, firstName: String, lastName: String)

object Patient extends MongoRecord {
  object patient extends Meta[Patient] {
    object id extends StringField(this)
    object snils extends IntField(this)
    object address_id extends StringField(this)
  }
  object address extends Meta[Address] {
    object id extends StringField(this)
    object street extends StringField(this)
  }
  object personalData extends Meta[PersonalData] {
    object person_id extends StringField(this)
    object firstName extends IntField(this)
    object lastName extends IntField(this)
  }

  def init():Unit = {

    address.where.remove
    address.insert(Address("1", "K")).flash

    patient.where.remove
    patient.insert(Patient("tim", "123", "1")).flash
    patient.insert(Patient("tim", "123", "0")).flash

    personalData.where.remove
    personalData.insert(PersonalData("tim", "tim", "bay")).flash
    personalData.insert(PersonalData("tim", "tim", "bay")).flash
  }

  def allInfo = join(patient, personalData) { (p, pd) =>
    where(p.id === "tim") on(p.id === pd.person_id)
  }

  def hashMany = join(patient, personalData) { (p, pd) =>
    where(p.id === "tim") on(p.id << pd.person_id)
  }

  def triple = join(patient, personalData, address) { (p, pd, ad) =>
    where(p.id === "tim") on(p.id << pd.person_id, p.address_id === ad.id)
  }

}

class GroupTest extends Spec {

  Patient.init()

  Patient.allInfo.fetch.length shouldBe 2

  Patient.hashMany.fetch.length shouldBe 2

  Patient.triple.fetch.length shouldBe 2

  yes(Patient.triple.fetchOne, (Patient("tim","123","1"),List(PersonalData("tim","tim","bay"), PersonalData("tim","tim","bay")),Some(Address("1","K"))))

}

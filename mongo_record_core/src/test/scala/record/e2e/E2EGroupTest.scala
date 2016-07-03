package record.e2e

import record.{imports, MongoRecord, Spec}
import imports._
/**
 * Created by tim on 17.05.16.
 */
case class Patient(id: String, snils: String, address_id: String)
case class Address2(id: String, street: String)
case class PersonalData(person_id: String, firstName: String, lastName: String)
object Patient extends MongoRecord {
  object patient extends Meta[Patient] {
    object id extends StringField(this)
    object snils extends IntField(this)
    object address_id extends StringField(this)
  }
  object address extends Meta[Address2] {
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
    address.insert(Address2("1", "K")).flash

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

class E2EGroupTest extends Spec {

  Patient.init()

  Patient.allInfo.fetch.foreach {println}

  Patient.hashMany.fetch.foreach {println}

  Patient.triple.fetch.foreach {println}

  Patient.triple.fetchOne.foreach {println}

}

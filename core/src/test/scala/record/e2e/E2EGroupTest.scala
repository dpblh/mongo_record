package record.e2e

import record.{MongoRecord, Spec}
import record.imports._
/**
 * Created by tim on 17.05.16.
 */
case class Patient(id: String, snils: String)
case class PersonalData(person_id: String, firstName: String, lastName: String)
object Patient extends MongoRecord {
  object patient extends Meta[Patient] {
    override val collection_name: String = "patient"
    object id extends StringField("id", this)
    object snils extends IntField("snils", this)
  }
  object personalData extends Meta[PersonalData] {
    override val collection_name: String = "personal_data"
    object person_id extends StringField("person_id", this)
    object firstName extends IntField("firstName", this)
    object lastName extends IntField("lastName", this)
  }

  def initCollection():Unit = {
    patient.where.remove
    patient.insert(Patient("tim", "123")).flash
    patient.insert(Patient("tim", "123")).flash

    personalData.where.remove
    personalData.insert(PersonalData("tim", "tim", "bay")).flash
    personalData.insert(PersonalData("tim", "tim", "bay")).flash
  }

  def allInfo = join(patient, personalData) { (p, pd) =>
    where(p.id === "tim") on(p.id === pd.person_id)
  }

  def hashOne = join(patient, personalData) { (p, pd) =>
    where(p.id === "tim") on(p.id hashOne pd.person_id)
  }

  def hashMany = join(patient, personalData) { (p, pd) =>
    where(p.id === "tim") on(p.id hashMany pd.person_id)
  }

}

class E2EGroupTest extends Spec {

  Patient.initCollection()

  Patient.allInfo.fetch.foreach {println}

  Patient.hashOne.fetch.foreach {println}

  Patient.hashMany.fetch.foreach {println}

}

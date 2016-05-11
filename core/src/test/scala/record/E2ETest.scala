package record

import java.util.Date

import org.scalatest.{Matchers, FreeSpec}
import record.MongoRecord
import record.imports._

/**
 * Created by tim on 09.05.16.
 */

class E2ETest extends FreeSpec with Matchers with MongoRecord {

  case class Person(id: String, name: String, age: Int)

  case class Fr(name: String, person: Person, created_at: Date)

  object Person extends Meta[Person] {

    override val collection_name: String = "person"
    object name extends StringField("name", this)
    object age extends IntField("age", this)

  }

  Person { p =>
    where(p.name === "tim") select p
  }.fetch

}

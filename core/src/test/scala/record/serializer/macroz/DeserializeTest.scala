package record.serializer.macroz

import scala.language.experimental.macros
import record.Spec
import record.macroz.serializer.DBObjectSerializer._
/**
 * Created by tim on 12.06.16.
 */
class DeserializeTest extends Spec {

  def fromDBObject[T]: Any => T = macro fromDBObjectImpl[T]

  val address = com.mongodb.BasicDBObjectBuilder
    .start
    .append("region", "Kal")
    .append("city", "Tver")
    .get()

  println(fromDBObject[Address](address))

}

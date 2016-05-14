package record

import com.mongodb.DBObject
import scala.reflect.runtime.universe._
/**
 * Created by tim on 09.05.16.
 */
trait Query {
  def execute:execute
}
sealed class execute
case class selectExecute[T](collection: String, condition: DBObject, select: DBObject, ev1: Type) extends execute
case class updateExecute[T](collection: String, condition: DBObject, update: DBObject) extends execute

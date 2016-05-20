package record

import com.mongodb.DBObject
/**
 * Created by tim on 09.05.16.
 */
trait Query {
  def execute:execute
}
sealed class execute
case class selectExecute(collection: String, condition: DBObject, transform: DBObject => Any) extends execute
case class selectFieldsExecute(collection: String, condition: DBObject, select: DBObject, transform: DBObject => Any) extends execute
case class insertExecute(collection: String, toBeInsert: DBObject) extends execute
case class conditionExecute(collection: String, condition: DBObject) extends execute
case class updateExecute(collection: String, condition: DBObject, update: DBObject) extends execute
case class joinExecute(collection: String, aggregate: List[DBObject], transform: DBObject => Any) extends execute

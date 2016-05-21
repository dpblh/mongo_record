package record

import com.mongodb.DBObject
/**
 * Created by tim on 09.05.16.
 */
trait Query[R] {
  def execute:execute[R]
}
sealed class execute[R]
case class selectExecute[R](collection: String, condition: DBObject, transform: DBObject => R) extends execute[R]
case class selectFieldsExecute[R](collection: String, condition: DBObject, select: DBObject, transform: DBObject => R) extends execute[R]
case class insertExecute(collection: String, toBeInsert: DBObject) extends execute[Any]
case class conditionExecute(collection: String, condition: DBObject) extends execute[Any]
case class updateExecute(collection: String, condition: DBObject, update: DBObject) extends execute[Any]
case class joinExecute(collection: String, aggregate: List[DBObject], transform: DBObject => Any) extends execute[Any]

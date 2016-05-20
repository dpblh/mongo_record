package record

import com.mongodb.{BasicDBList, DBObject}
import scala.reflect.runtime.universe._
/**
 * Created by tim on 09.05.16.
 */
trait Query {
  def execute:execute
}
sealed class execute
case class selectExecute[T](collection: String, condition: DBObject, entityType: Type) extends execute
case class selectFieldsExecute[T](collection: String, condition: DBObject, select: DBObject, fields: List[Lexis#Field[_,_]]) extends execute
case class insertExecute[T](collection: String, toBeInsert: DBObject) extends execute
case class conditionExecute[T](collection: String, condition: DBObject) extends execute
case class updateExecute[T](collection: String, condition: DBObject, update: DBObject) extends execute
case class joinExecute[T](collection: String, aggregate: List[DBObject], entityType: Type, joins: List[Lexis#Join[_,_,_]]) extends execute

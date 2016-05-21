package record

import record.signatures._
import MongoBuilder._
import scala.reflect.runtime.universe._
import com.mongodb.DBObject
/**
 * Created by tim on 09.05.16.
 */
trait Query[R] {
  def execute:execute[R]
}

case class SelectQuery[R](c: M, s: SelectState[R], tag: Type) extends Query[R] {
  override def toString =       selectQueryAsString(this)
  override def execute  =        selectQuery(this)
}

case class JoinQuery[R](joined: JoinStateYield[R], collection: M) extends Query[R] {
  override def toString =       joinQueryAsString(this)
  override def execute  =       joinQuery(this)
}

case class ModifyQuery[T <: M](c: T, s: ModifyState[_]) extends Query[Boolean] {
  override def toString =       modifyQueryAsString(this)
  override def execute  =       modifyQuery(this)
}

case class WhereQuery[C](w: WhereState[C], collection: M) extends Query[Boolean] {
  override def execute =        whereQuery(this)
}

case class InsertQuery[C](t: MetaTag[C], c: Any) extends Query[Boolean] {
  override def toString =       insertQueryAsString(this)
  override def execute  =       insertQuery(this)
}

sealed class execute[R]
case class selectExecute[R]         (collection: String, condition: DBObject, transform: DBObject => R) extends execute[R]
case class selectFieldsExecute[R]   (collection: String, condition: DBObject, select: DBObject, transform: DBObject => R) extends execute[R]
case class insertExecute            (collection: String, toBeInsert: DBObject) extends execute[Boolean]
case class conditionExecute         (collection: String, condition: DBObject) extends execute[Boolean]
case class updateExecute            (collection: String, condition: DBObject, update: DBObject) extends execute[Boolean]
case class joinExecute[R]           (collection: String, aggregate: List[DBObject], transform: DBObject => R) extends execute[R]

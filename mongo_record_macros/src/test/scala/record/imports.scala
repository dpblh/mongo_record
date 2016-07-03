package record

import com.mongodb.{DBObject, MongoClient}

/**
 * Created by tim on 09.05.16.
 */
object imports extends MongoRecordMacro {

  object DBAdapter {
    import scala.collection.JavaConversions._
    val mongoClient = new MongoClient()
    val db = mongoClient.getDB("test_record")
    def fetch[R](query: Query[R]):List[R] = {
      query.execute match {
        case selectExecute(collection, condition, transform) =>
          db.getCollection(collection).find(condition).toArray.toList.map(transform)
        case selectFieldsExecute(collection, condition, select, transform) =>
          db.getCollection(collection).find(condition, select).toArray.toList.map(transform)
        case joinExecute(collection, aggregate, transform) =>
          db.getCollection(collection).aggregate(aggregate).results().toArray.toList.map(transform)
        case conditionExecute(collection, condition, transform) =>
          db.getCollection(collection).find(condition).toArray.toList.map(transform)
      }
    }
    def fetchOne[R](query: Query[R]):Option[R] = {
      query.execute match {
        case selectExecute(collection, condition, transform) =>
          db.getCollection(collection).findOne(condition) match {
            case x: DBObject => Some(transform(x))
            case _ => None
          }
        case selectFieldsExecute(collection, condition, select, transform) =>
          db.getCollection(collection).findOne(condition, select) match {
            case x: DBObject => Some(transform(x))
            case _ => None
          }
        case joinExecute(collection, aggregate, transform) =>
          db.getCollection(collection).aggregate(aggregate).results().toArray.toList.headOption.map(transform)
      }
    }
    def update(query: Query[_], multi: Boolean):Boolean = {
      query.execute match {
        case updateExecute(collection, condition, update) =>
          db.getCollection(collection).update(condition, update, false, multi)
          true
      }
    }
    def count(query: Query[_]):Long = {
      query.execute match {
        case conditionExecute(collection, condition, transform) =>
          db.getCollection(collection).count(condition)
      }
    }
    def remove(query: Query[_]):Unit = {
      query.execute match {
        case conditionExecute(collection, condition, transform) =>
          db.getCollection(collection).remove(condition)
      }
    }
    def insert(query: Query[_]):Boolean = {
      query.execute match {
        case insertExecute(collection, toBeInsert) =>
          db.getCollection(collection).insert(toBeInsert)
          true
      }
    }
  }

  class DBExecutor {
    def fetch[R](query: Query[R]) = DBAdapter.fetch(query)
    def fetchOne[R](query: Query[R]) = DBAdapter.fetchOne(query)
    def update(query: Query[_], multi: Boolean = false) = DBAdapter.update(query, multi)
    def count(query: Query[_]) = DBAdapter.count(query)
    def remove(query: Query[_]) = DBAdapter.remove(query)
    def insert(query: Query[_]) = DBAdapter.insert(query)
  }

  object ImplDBExecutor extends DBExecutor

  case class MongoQueryInsert[R](q: InsertQuery[R], db: DBExecutor) {
    def flash = db.insert(q)
  }

  case class MongoQueryJoin[R](q: JoinQuery[R], db: DBExecutor) {
    def fetch = db.fetch(q)
    def fetchOne = db.fetchOne(q)
  }

  case class MongoQueryModify[T <: M](q: ModifyQuery[T], db: DBExecutor) {
    def modify() = db.update(q, multi = true)
    def modifyOne() = db.update(q)
  }

  case class MongoQuerySelect[R](q: SelectQuery[R], db: DBExecutor) {
    def fetch = db.fetch(q)
    def fetchOne = db.fetchOne(q)
  }

  case class MongoQueryWhere[R](q: WhereQuery[R], db: DBExecutor) {
    def fetch = db.fetch(q)
    def fetchOne = db.fetchOne(q)
    def count = db.count(q)
    def remove = db.remove(q)
  }

  implicit def insertQuery2MongoRecord[R](q: InsertQuery[R]) = MongoQueryInsert(q, ImplDBExecutor)
  implicit def joinQuery2MongoRecord[R](q: JoinQuery[R]) = MongoQueryJoin(q, ImplDBExecutor)
  implicit def selectQuery2MongoRecord[R](q: SelectQuery[R]) = MongoQuerySelect(q, ImplDBExecutor)
  implicit def modifyQuery2MongoRecord[T <: M](q: ModifyQuery[T]) = MongoQueryModify(q, ImplDBExecutor)
  implicit def whereQuery2MongoRecord[R](q: WhereQuery[R]) = MongoQueryWhere(q, ImplDBExecutor)
  implicit def singleRecord2Meta[T](s: SingleRecord { def mt:T }) = s.mt

}

package record

import com.mongodb.{DBObject, MongoClient}

/**
 * Created by tim on 09.05.16.
 */
object imports {

  object DBAdapter {
    import scala.collection.JavaConversions._
    val mongoClient = new MongoClient()
    val db = mongoClient.getDB("test_record")
    def fetch(query: Query):List[_] = {
      query.execute match {
        case selectExecute(collection, condition, transform) =>
          db.getCollection(collection).find(condition).toArray.toList.map(transform)
        case selectFieldsExecute(collection, condition, select, transform) =>
          db.getCollection(collection).find(condition, select).toArray.toList.map(transform)
        case joinExecute(collection, aggregate, transform) =>
          db.getCollection(collection).aggregate(aggregate).results().toArray.toList.map(transform)
      }
    }
    def fetchOne(query: Query):Option[_] = {
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
      }
    }
    def update(query: Query, multi: Boolean):Unit = {
      query.execute match {
        case updateExecute(collection, condition, update) =>
          db.getCollection(collection).update(condition, update, false, multi)
      }
    }
    def count(query: Query):Long = {
      query.execute match {
        case conditionExecute(collection, condition) =>
          db.getCollection(collection).count(condition)
      }
    }
    def remove(query: Query):Unit = {
      query.execute match {
        case conditionExecute(collection, condition) =>
          db.getCollection(collection).remove(condition)
      }
    }
    def insert(query: Query):Unit = {
      query.execute match {
        case insertExecute(collection, toBeInsert) =>
          db.getCollection(collection).insert(toBeInsert)
      }
    }
  }

  class DBExecutor {
    def fetch(query: Query) = DBAdapter.fetch(query)
    def fetchOne(query: Query) = DBAdapter.fetchOne(query)
    def update(query: Query, multi: Boolean = false) = DBAdapter.update(query, multi)
    def count(query: Query) = DBAdapter.count(query)
    def remove(query: Query) = DBAdapter.remove(query)
    def insert(query: Query) = DBAdapter.insert(query)
  }
  object ImplDBExecutor extends DBExecutor

  case class MongoRecordReader(q: Query, db: DBExecutor) {
    def fetch = db.fetch(q)
    def fetchOne = db.fetchOne(q)
    def modify() = db.update(q, multi = true)
    def modifyOne() = db.update(q)
    def count = db.count(q)
    def remove = db.remove(q)
    def flash = db.insert(q)
  }

  implicit def query2MongoRecord(q: Query):MongoRecordReader = MongoRecordReader(q, ImplDBExecutor)

}

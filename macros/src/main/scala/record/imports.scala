package record

import com.mongodb.{BasicDBList, DBObject, MongoClient}
import record.joinExecute

import scala.collection.mutable

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
        case selectExecute(collection, condition, entityType) =>
          db.getCollection(collection).find(condition).toArray.toList.map(DBObjectSerializer.fromDBObjectType(_, entityType))
        case selectFieldsExecute(collection, condition, select, fields) =>
          db.getCollection(collection).find(condition, select).toArray.toList.map { dbobject =>
            fields.map { field =>
              DBObjectSerializer.fromDBObjectType(dbobject.get(field.fieldName), field.typeOf2)
            }
          }
        case joinExecute(collection, aggregate, entityType, joins) =>
          db.getCollection(collection).aggregate(aggregate).results().toArray.toList.map { m =>
            val head = DBObjectSerializer.fromDBObjectType(m, entityType)
            val tail = joins.map { join =>
              val joinCollection = m.get(join.joined.collection.toString).asInstanceOf[BasicDBList]
              join match {
                case y: Lexis#JoinOne[_,_,_] =>
                  joinCollection match {
                    case x if x.nonEmpty =>
                      val h = DBObjectSerializer.fromDBObjectType(x.head, y.joined.collection.asInstanceOf[Lexis#Meta[_]].typeOf2)
                      Some(h)
                    case _ => None
                  }
                case y: Lexis#JoinMany[_,_,_] =>
                  joinCollection.toList.map { j =>
                    DBObjectSerializer.fromDBObjectType(j, y.joined.collection.asInstanceOf[Lexis#Meta[_]].typeOf2)
                  }
              }
            }
            head::tail
          }
      }
    }
    def fetchOne(query: Query):Option[_] = {
      query.execute match {
        case selectExecute(collection, condition, entityType) =>
          db.getCollection(collection).findOne(condition) match {
            case x: DBObject => Some(DBObjectSerializer.fromDBObjectType(x, entityType))
            case _ => None
          }
        case selectFieldsExecute(collection, condition, select, fields) =>
          db.getCollection(collection).findOne(condition, select) match {
            case x: DBObject => Some(
              fields.map { field =>
                DBObjectSerializer.fromDBObjectType(x.get(field.fieldName), field.typeOf2)
              }
            )
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

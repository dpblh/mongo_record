package record

import com.mongodb.MongoClient

import scala.collection.mutable

/**
 * Created by tim on 09.05.16.
 */
object imports {

  import scala.reflect.runtime.universe._

  object DBAdapter {
    import scala.collection.JavaConversions._
    val mongoClient = new MongoClient()
    val db = mongoClient.getDB("test_record")
    def fetch(query: Query):mutable.Buffer[_] = {
      query.execute match {
        case selectExecute(collection, condition, select, ev1) =>
          if (select.keySet().isEmpty)
            db.getCollection(collection).find(condition).toArray.map(DBObjectSerializer.fromDBObjectType(_, ev1))
          else
            db.getCollection(collection).find(condition, select).toArray.map(DBObjectSerializer.fromDBObjectType(_, ev1))
      }
    }
  }

  class DBExecutor {
    def fetch(query: Query) = DBAdapter.fetch(query)
    def fetchOne(query: Query) = ???
    def count(query: Query) = ???
  }
  object ImplDBExecutor extends DBExecutor

  case class MongoRecordReader(q: Query, db: DBExecutor) {
    def fetch = db.fetch(q)
    def fetchOne = db.fetchOne(q)
    def count = db.count(q)
  }

  implicit def query2MongoRecord(q: Query):MongoRecordReader = MongoRecordReader(q, ImplDBExecutor)
  implicit def any2typeTag[A: TypeTag](any: AnyRef):TypeTag[A] = typeTag[A]

}

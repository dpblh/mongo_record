package record

/**
 * Created by tim on 09.05.16.
 */
object imports {

  import scala.reflect.runtime.universe._

  class DBExecutor {
    def fetch(query: Query) = ???
    def fetchOne(query: Query) = ???
    def count(query: Query) = ???
  }
  object ImplDBExecutor extends DBExecutor

  case class MongoRecord(q: Query, db: DBExecutor) {
    def fetch = db.fetch(q)
    def fetchOne = db.fetchOne(q)
    def count = db.count(q)
  }

  implicit def query2MongoRecord(q: Query):MongoRecord = MongoRecord(q, ImplDBExecutor)
  implicit def any2typeTag[A: TypeTag](any: AnyRef):TypeTag[A] = typeTag[A]

}

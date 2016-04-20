package record

/**
 * Created by tim on 19.04.16.
 */
class MongoRecordImpl extends MongoRecord {
  override def classAsString[C <: AnyRef](c: C):String = "{'name': 'tim', 'fio': 'bay', 'age': 23}"
}

object MongoRecordImpl extends MongoRecordImpl
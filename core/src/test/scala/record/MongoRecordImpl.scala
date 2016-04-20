package record

/**
 * Created by tim on 19.04.16.
 */
trait MongoRecordImpl extends MongoRecord {
  override def classAsString[C <: AnyRef](c: C):String = "{'name': 'tim', 'fio': 'bay', 'age': 23}"
}

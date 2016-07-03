package record

import record.macroz.serializer.DBObjectSerializer.mongo_mapper
import record.macroz.serializer.SerializerUtils

import scala.language.experimental.macros

/**
 * Created by tim on 18.04.16.
 */
trait MongoRecordMacro
  extends record.MongoRecord
  with MacroWrapper {

  def meta[T]: MetaTag[T] = macro SerializerUtils.metaGenerator[T]
  def mapper[T]: Mapper[T] = macro mongo_mapper[T]

  trait Mapper[T] {
    def to(e: T):Any
    def from(o: Any):T
  }

}
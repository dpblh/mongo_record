package record

import record.macroz.serializer.SerializerUtils
import record.signatures._

import scala.language.experimental.macros

/**
 * Created by tim on 18.04.16.
 */
trait MongoRecord
  extends BaseFields
  with FromSignatures
  with JoinSignatures
  with ModifySignatures
  with WhereSignatures
  with MacroWrapper {

  type Meta[C] = ObjectMetaTag[C]

  def meta[T]: MetaTag[T] = macro SerializerUtils.metaGenerator[T]
  object mongo {
    def from[T]: Any => T = macro macroz.serializer.DBObjectSerializer.fromDBObjectImpl[T]
    def as[T]: T => Any = macro macroz.serializer.DBObjectSerializer.asDBObjectImpl[T]
  }

}
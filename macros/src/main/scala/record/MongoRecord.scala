package record

import record.macroses.serializer.DBObjectSerializer
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

  def meta[T]: MetaTag[T] = macro DBObjectSerializer.metaGenerator[T]

}
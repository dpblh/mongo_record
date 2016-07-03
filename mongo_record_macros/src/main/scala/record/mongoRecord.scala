package record

import record.macroz.serializer.MongoRecordImpl

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros

/**
 * Created by tim on 03.07.16.
 */
@compileTimeOnly("enable macro paradise to expand macro annotations")
class mongoRecord extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MongoRecordImpl.impl
}

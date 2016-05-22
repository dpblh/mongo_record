package record

import record.signatures._
import record.DBObjectSerializer._
/**
 * Created by tim on 22.05.16.
 */
trait MacroWrapper {

  abstract class SingleRecord[C] {
    val meta: MetaTag[C]
    type it = MetaTag[C]

    def insert(c: C) =                      InsertQuery(meta, asDBObject(c, meta.runtimeClass))
    def modify(c1: it => ModifyState[_]) =  ModifyQuery(meta, c1(meta))

    def where(c1: Expression[C]) =          WhereState(c1)
    def where =                             WhereQuery(WhereState(allExpression[C]()), meta)
    def where(c1: it => Expression[C]) =    WhereQuery(WhereState(c1(meta)), meta)

    def find[R](c1: it => SelectState[R]) = SelectQuery[R](meta, c1(meta), meta.runtimeClass)

  }

}

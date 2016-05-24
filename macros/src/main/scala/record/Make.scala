package record

import record.signatures._
import DBObjectSerializer._
import scala.reflect.runtime.universe._
/**
 * Created by tim on 21.05.16.
 */
trait Make[C]

abstract class MetaTag[C: TypeTag] extends Make[C] with BaseFields {

  type it = this.type

  val collection_name:String

  def isValid(c: C):Boolean = true

  def insert(c: C) =                      InsertQuery(this, asDBObject(c, runtimeClass))
  def modify(c1: it => ModifyState[_]) =  ModifyQuery(this, c1(this))

  def where(c1: Expression[C]) =          WhereState(c1)
  def where =                             WhereQuery(WhereState(allExpression[C]()), this)
  def where(c1: it => Expression[C]) =    WhereQuery(WhereState(c1(this)), this)

  def find[R](c1: it => SelectState[R]) = SelectQuery[R](this, c1(this), runtimeClass)

  def dynamic[F](field: String) =         DynamicField[C, F](field, this)

  override def toString = collection_name

  def runtimeClass: Type = typeOf[C]
}
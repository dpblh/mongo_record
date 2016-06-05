package record

import record.serializer.DBObjectSerializer
import record.signatures._
import scala.reflect.runtime.universe._
/**
 * Created by tim on 21.05.16.
 */
trait Make[C] {
  def asDBObject(c: Any):Any
  def fromDBObject(dbo: Any):Any

  val originName:String
  val entityName:String

  //TODO concurrency variable
  var fields:Map[String, Make[_]] = null
  def getField(field: String):Make[_] = {
    if (fields == null) {
      fields = ReflectionRecord.getMetaFields(getClass)
    }
    fields(field)
  }

}

abstract class MetaTag[C: TypeTag] extends Make[C] {

  type it = this.type

  def isValid(c: C):Boolean = true

  def insert(c: C)                        =  InsertQuery(this, asDBObject(c))
  def modify(c1: it => ModifyState[_])    =  ModifyQuery(this, c1(this))

  def where(c1: Expression[C])            =  WhereState(c1)
  def where                               =  WhereQuery(WhereState(allExpression[C]()), this)
  def where(c1: it => Expression[C])      =  WhereQuery(WhereState(c1(this)), this)

  def find[R](c1: it => SelectState[R])   =  SelectQuery[R](this, c1(this), runtimeClass)

  def dynamic[F: TypeTag](field: String)  =  RuntimeField[C, F](field, this)

  override def toString = entityName

  def asDBObject(c: Any):Any              =  DBObjectSerializer.asDBObject(c, runtimeClass, Some(this))
  def fromDBObject(dbo: Any):C            =  DBObjectSerializer.fromDBObject(dbo, runtimeClass, Some(this)).asInstanceOf[C]
  def runtimeClass: Type                  =  typeOf[C]

}

abstract class ObjectMetaTag[C: TypeTag] extends MetaTag[C] {
  override val originName:String = ReflectionRecord.getName(getClass)
  override val entityName:String = ReflectionRecord.camelToUnderscores(originName)
}
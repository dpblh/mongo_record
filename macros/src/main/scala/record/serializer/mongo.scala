package record.serializer

import java.util.{Calendar, Date}

import com.mongodb.{BasicDBList, BasicDBObjectBuilder}
import record.serializer.DBObjectSerializer._

import scala.reflect.runtime.universe._
/**
 * Created by tim on 03.06.16.
 */
object mongo {

  def asSimpleType(o: Any):Any = o match {
    case x: BigInt              => x.toString()
    case x: BigDecimal          => x.toString()
    case x => x
  }

  def asDate(o: Any):Any = o match {
    case x: Date          => x.getTime
    case x: Calendar      => x.getTimeInMillis
  }

  def asOption(o: Any, tup: Type):Any = o match {
    case Some(x)   => asDBObject(x, tup.typeArgs.head)
    case None      => null
  }

  def asCollection(o: Any, tup: Type):Any = {
    val list = new BasicDBList()
    o.asInstanceOf[Iterable[_]].foreach { element =>
      list.add(asDBObject(element, tup.typeArgs.head).asInstanceOf[AnyRef])//TODO AnyRef ?
    }
    list
  }

  def asMap(o: Any, tup: Type):Any = {
    val builder = BasicDBObjectBuilder.start()
    o.asInstanceOf[Map[String,_]].foreach { tupl =>
      val (key, value) = tupl
      builder.append(key, asDBObject(value, tup.typeArgs(1)))
    }
    builder.get()
  }

  def asCase(o: Any, tup: Type):Any = {
    val builder = BasicDBObjectBuilder.start()
    val mirror = runtimeMirror(o.getClass.getClassLoader)
    val xm = mirror reflect o
    tup.decls.collect {
      case acc: MethodSymbol if acc.isCaseAccessor =>
        val value = (xm reflectMethod acc)()
        (acc.name.decodedName.toString, asDBObject(value, acc.returnType))
    } foreach { a => builder.append(a._1, a._2) }
    builder.get()
  }

}

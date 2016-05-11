package record

import java.util.{Calendar, Date}
import scala.reflect.runtime.universe._
import com.mongodb.{BasicDBObjectBuilder, DBObject}

/**
 * Created by tim on 11.05.16.
 */
object DBObjectSerializer {
  def asDBObjectTypeTag[T](entity: T)(implicit tag: TypeTag[T]):DBObject = {
    val mirror = runtimeMirror(entity.getClass.getClassLoader)

    def a2dbObject(x: Any, t: Type): Any = {
      val xm = mirror reflect x
      val members = t.decls.collect {
        case acc: MethodSymbol if acc.isCaseAccessor => fieldAsTuple(acc, xm)
      }

      if (members.isEmpty) x else {
        val builder = BasicDBObjectBuilder.start()

        members.foreach { a => builder.append(a._1, a._2) }
        builder.get()
      }
    }

    def fieldAsTuple(acc: MethodSymbol, xm: InstanceMirror):(String, Any) = {
      val value = (xm reflectMethod acc)()
      val returnValue = acc.returnType match {

        case x if primitive_?(x) => value
        case x if date_?(x) => any2DBDate(value)
        case x => a2dbObject(value, acc.typeSignature)

      }
      (acc.name.decodedName.toString, returnValue)
    }

    a2dbObject(entity, typeOf[T]).asInstanceOf[DBObject]

  }

  def asDBObject[A: TypeTag](entity: A):DBObject = asDBObjectTypeTag(entity)(typeTag[A])

  def date_?(`type`: Type): Boolean = dates.exists(_ =:= `type`)
  def primitive_?(`type`: Type): Boolean = primitives.exists(_ =:= `type`)

  def any2DBDate(o: Any):Any = {
    o match {
      case x: Date     => x.getTime
      case x: Calendar => x.getTimeInMillis
    }
  }

  val dates = Set(typeOf[Date])
  val primitives = Set[Type](typeOf[String], typeOf[Int], typeOf[Long], typeOf[Double],
    typeOf[Float], typeOf[Byte], typeOf[BigInt], typeOf[Boolean],
    typeOf[Short], typeOf[java.lang.Integer], typeOf[java.lang.Long],
    typeOf[java.lang.Double], typeOf[java.lang.Float],
    typeOf[java.lang.Byte], typeOf[java.lang.Boolean],
    typeOf[java.lang.Short], typeOf[scala.Array[Byte]])


}
package record.macroz.serializer

import record.macroz.serializer.DBObjectSerializer._
import record.macroz.serializer.SerializerUtils._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Created by tim on 10.06.16.
 */
object mongo {

  def asCase(c: Context)(tpe: c.universe.Type, root: c.Tree): c.Tree = {
    import c.universe._

    val fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (entity, name, typ) = p
      val value = asDBObject(c)(typ, Select(root, TermName(name.decodedName.toString)))
      q"builder.append($entity, $value)"
    }

    q"""{
        val builder = com.mongodb.BasicDBObjectBuilder.start()
        ..$fields
        builder.get()
        }
     """
  }

  def asCollection(c: Context)(tpe: c.universe.Type, name: c.Tree) = {
    import c.universe._
    val elementSerializer = asDBObject(c)(tpe.typeArgs.head, q"element")
    q"""{
        val list = new com.mongodb.BasicDBList()
          $name.foreach { element =>
            list.add($elementSerializer)
          }
       list
       }
     """
  }

  def asMap(c: Context)(tpe: c.universe.Type, name: c.Tree) = {
    import c.universe._
    q"""
       {
        val builder = com.mongodb.BasicDBObjectBuilder.start()
        $name.foreach { t =>
          val (key, _) = t
          val value = record.runtime.serializer.DBObjectSerializer.asDBObject[${tpe.typeArgs(1)}](t._2)
          builder.append(key, value)
        }
        builder.get()
       }
     """
  }

  def asOption(c: Context)(tpe: c.universe.Type, name: c.Tree) = {
    import c.universe._
    q"""
       $name match {
        case Some(x) => ${asDBObject(c)(tpe.typeArgs.head, q"x")}
        case None => null
       }
     """
  }

  def asDate(c: Context)(tpe: c.universe.Type, name: c.Tree) = {
    import c.universe._
    tpe.typeSymbol.name.toString match {
      case "Date" => q"$name.getTime"
      case "Calendar" => q"$name.getTimeInMillis"
    }
  }

  def asSimpleType(c: Context)(tpe: c.universe.Type, name: c.Tree): c.Tree = {
    import c.universe._
    tpe.typeSymbol.name.toString match {
      case "BigInt" => q"$name.toString"
      case "BigDecimal" => q"$name.toString"
      case _ => q"$name"
    }
  }

}

package record.macroses.serializer

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

/**
 * Created by tim on 10.06.16.
 */
object mongo {

  import DBObjectSerializer._

  def asCase(c: Context)(tpe: c.universe.Type, root: c.Tree): c.Tree = {
    import c.universe._

    val fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (name, typ) = p
      val value = asDBObject(c)(typ, Select(root, TermName(name.decodedName.toString)))
      q"builder.append(${name.toString}, $value)"
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
    //TODO
    q"""{
        val list = new BasicDBList()
          $name.foreach { element =>
            list.add(record.serializer.DBObjectSerializer.asDBObject[${tpe.typeArgs.head}](element))
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
          val value = record.serializer.DBObjectSerializer.asDBObject[$tpe](t._2)
          builder.append(key, value)
        }
        builder.get()
       }
     """
  }

  def asOption(c: Context)(tpe: c.universe.Type, name: c.Tree) = {
    import c.universe._
    q"""
       name match {
        case Some(x) => ${asDBObject(c)(tpe.typeArgs.head, name)}
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
      case "Int" => q"$name"
      case "String" => q"$name"
    }
  }

}

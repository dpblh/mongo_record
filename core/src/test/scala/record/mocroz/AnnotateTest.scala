package record.mocroz

import record.imports._
import record.Spec
import record.macroz.serializer.{entityName, mongoRecord}
import com.mongodb.util.JSON._

/**
 * Created by tim on 25.06.16.
 */


@mongoRecord @entityName(name = "clazz3") case class Clazz2(x: String)
@mongoRecord case class Point2(@entityName(name = "xxx") x: Int, @entityName(name = "clazz3")clazz: Clazz2)
object Point2 extends Meta {
  val one = 3
}

class AnnotateTest extends Spec {

  Clazz2("77788s8asad891sdd45d6d1d0sadssaasdsddadasdasddddsddasdasaasddsaadsdsadsdasasdsaSd")

  println(Clazz2.x)
  println(Clazz2.entityName)

  println(Clazz2.asDBObject(Clazz2("1sdasd231as2313")))

  println(Point2.one)
  println(Point2.x)
  println(Point2.clazz.x)
  println(Point2(2, Clazz2("3")).save())
  println(Point2.asDBObject(Point2(2, Clazz2("3"))))
  println(Point2.fromDBObject(parse("""{ "xxx" : 2 , "clazz3" : { "x" : "3"}}""")))

}

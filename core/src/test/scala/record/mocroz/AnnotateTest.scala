package record.mocroz

import record.imports._
import record.Spec
import record.macroz.serializer.mongoRecord
import com.mongodb.util.JSON._

/**
 * Created by tim on 25.06.16.
 */


@mongoRecord case class Clazz2(x: String)
@mongoRecord case class Point2(x: Int, clazz: Clazz2)
object Point2 extends Meta {
  val one = 1
  object x extends StringField(this) {
    override val entityName = "x_1"
  }
}

class AnnotateTest extends Spec {

  Clazz2("")

  println(Clazz2.x)
  println(Clazz2.entityName)

  println(Clazz2.asDBObject(Clazz2("12312313")))

  println(Point2.one)
  println(Point2.x)
  println(Point2(2, Clazz2("3")).save())
  println(Point2.asDBObject(Point2(2, Clazz2("3"))))
  println(Point2.fromDBObject(parse("""{ "x" : 2 , "clazz" : { "x" : "3"}}""")))

}

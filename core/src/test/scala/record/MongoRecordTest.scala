package record

import org.scalatest.{Matchers, FreeSpec}
import query.{Raw, UtilsMacro}

import scala.reflect._

/**
 * Created by tim on 16.04.16.
 */
class MongoRecordTest extends FreeSpec with Matchers { that =>

  case class Person(name: String, age: Int)

  object Person extends MongoRecord {

    val person = scheme[Person]

    from(person){ p =>
      where(p.age === 123) select p
    }

    def find = {
      from(person){ p =>
        where(p.age === 123) select p
      }
    }

  }

//  println(Person.find)

  import scala.reflect.runtime.{universe => ru}
  import ru._



  println(createInstance[P]())

  def createInstance[T:TypeTag]() : Any= {
//    val constructor = classOf[Class[T]].getConstructors()(0)
//
//    println(constructor.getParameterCount)
    createInstance(typeOf[T])
  }


  def createInstance(tpe:Type): Any = {


    object CaseField {
      def unapply(trmSym: TermSymbol): Option[String] = {
        if (trmSym.isVal && trmSym.isCaseAccessor)
          Some(trmSym.typeSignature.toString)
        else
          None
      }
    }

    val r = tpe.decls.collect {
      case CaseField(tpe) =>
        tpe match {
          case "String" => "noop"
          case "Int" => 0
          case x => null
        }
    }

    val mirror = ru.runtimeMirror(getClass.getClassLoader)
    val clsSym = tpe.typeSymbol.asClass
    val clsMirror = mirror.reflectClass(clsSym)
    val ctorSym = tpe.decl(ru.termNames.CONSTRUCTOR).asMethod
    val ctorMirror = clsMirror.reflectConstructor(ctorSym)
    val instance = ctorMirror(r.toSeq: _*)
    return instance
  }



  val a1 = Person("tim", 27)
  println(Raw.getName(a1.name))

}
case class P(n:String)
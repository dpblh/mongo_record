package query

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
 * Created by tim on 08.04.16.
 */
object Raw extends UtilsMacro {

  def raw(msg: Any) = macro rawImpl
  def rawType[T] = macro rawTypeImpl[T]

  def rawImpl(c: Context)(msg: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
//    println(showRaw(msg))
    println(showRaw(c.enclosingClass))
    throw new Error()

  }

  def rawTypeImpl[T: c.WeakTypeTag](c: Context): c.Expr[Unit] = {
    import c.universe._
    val tpe = weakTypeOf[T]

    val fields = getFieldNamesAndTypes(c)(tpe).foreach { p =>
      val (name, typ) = p

      println(isSimpleType(c)(typ))

//      typ match {
//        case TypeRef(ThisType(_), klass, List()) =>
//        case TypeRef(SingleType(ThisType(_), scala.Predef), TypeName(si), List()) =>
//      }
//      try {
//        typ
//        val TypeRef(ThisType(_), klass, List()) = typ
//        println(showRaw(klass.typeSignature))
//
//      } catch {
//        case e: Throwable =>
//      }

//      println(showRaw(name))
      println(showRaw(typ.typeSymbol.name))
    }

    throw new Error()
  }

  def getName(x: Any): String = macro impl

  def impl(c: Context)(x: c.Tree): c.Tree = {
    import c.universe._
    val p = x match {
      case Select(_, TermName(s)) => s
      case _ => ""
    }
    q"$p"
  }

}

package query

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
 * Created by tim on 08.04.16.
 */
object Raw {

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
    println(showRaw(tpe))
    throw new Error()
  }

}

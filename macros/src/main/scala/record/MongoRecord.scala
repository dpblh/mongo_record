package record

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
 * Created by tim on 18.04.16.
 */
trait MongoRecord extends Lexis {

  /**
   * просто карринг. для установки базового типа
   * @param c
   * @param c1
   * @tparam T Collection type
   * @return
   */
  def from[T <: M](c: T)(c1: T => SelectExpression): SelectResult[T] = SelectResult[T](c, c1(c))

  def join[T <: M, T1 <: M](c: T, c1: T1)(f: (T, T1) => Join[_, _, _]): JoinResult[T, T1] = JoinResult[T, T1](c, c1, f(c, c1))

  def join[T <: M, T1 <: M, T2 <: M](c: T, c1: T1, c2: T2)(f: (T, T1, T2) => Join[_, _, _]): JoinResult[T, T1] = JoinResult[T, T1](c, c1, f(c, c1, c2))

  def update[T <: M](c: T)(c1: T => UpdateExpression[T]): UpdateResult[T] = UpdateResult(c, c1(c))

  def mapReduce[T <: M](c: T)(c1: T => Reduce[_]): MapReduceResult[T] = MapReduceResult(c, c1(c))

  /**
   * predicate builder
   * @param c
   * @tparam C Collection
   * @return
   */
  def where[C](c: Expression[C]): WhereExpression[C] = WhereExpression(c)
  
}

object MongoRecord extends UtilsMacro {

  def meta[T]: Any = macro metaImpl[T]

  def metaImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val tpe = weakTypeOf[T]

    val collection_name = camelToUnderscores(tpe.typeSymbol.name.toString)

    var fields = getFieldNamesAndTypes(c)(tpe).map { p =>
      val (name, typ) = p
      q"val ${TermName(name.encoded)} = UField[$tpe, $typ](${name.encoded}, this)"
    }.toList

    fields = q"val collection_name = $collection_name" ::fields

    q"new Meta[$tpe] { ..$fields }"

  }

}
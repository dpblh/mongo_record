package record

import scala.reflect.runtime.universe._
/**
 * Created by tim on 01.06.16.
 */
object ReflectionRecord {

  def getName(clazz: Class[_]):String = {
    val fullName = clazz.getName
    val className = fullName.substring(fullName.lastIndexOf(".")+1)
    val simpleName = className.split("\\$")
    simpleName(simpleName.length - 1)
  }

  def getNameAsUnderscores(clazz: Class[_]):String = camelToUnderscores(getName(clazz))

  def camelToUnderscores(name: String) = name.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2").replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase

  def getMetaFields(clazz: Class[_]):Map[String, MakeRuntime[_]] = {
    val mirror = runtimeMirror(getClass.getClassLoader)

    mirror
      .classSymbol(clazz)
      .toType
      .members
      .filter(_.isModule)
      .map { m => mirror.reflectModule(m.asModule).instance.asInstanceOf[MakeRuntime[_]] }
      .map { d => (getNameAsUnderscores(clazz), d) }
      .toMap
  }

}

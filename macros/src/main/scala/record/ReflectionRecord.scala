package record

/**
 * Created by tim on 01.06.16.
 */
object ReflectionRecord {

  def getName(clazz: Class[_]):String = {
    val fullName = clazz.getName
    val className = fullName.substring(fullName.lastIndexOf(".")+1)
    val simpleName = className.split("\\$")
    camelToUnderscores(simpleName(simpleName.length - 1))
  }

  def camelToUnderscores(name: String) = name.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2").replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase

}

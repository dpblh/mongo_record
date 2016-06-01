package record

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
}

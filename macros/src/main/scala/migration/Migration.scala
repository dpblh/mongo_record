package migration

import java.text.SimpleDateFormat

/**
 * Created by tim on 07.05.16.
 */
trait Migration { self =>
  val className:String = self.getClass.getName
  val timestamp:Long
  val author:String
  def up(): Unit = {}
  def down(): Unit = {}
  protected def formatter(time: String) = new SimpleDateFormat("dd-MM-yyyy HH:mm").parse(time).getTime

  final override def equals(obj: scala.Any): Boolean = obj match {
    case x: Migration => x.className.equals(className)
    case _ => false
  }

  final override def hashCode(): Int = {
    17*className.hashCode
  }
}
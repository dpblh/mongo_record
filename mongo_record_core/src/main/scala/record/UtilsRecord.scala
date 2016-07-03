package record

/**
 * Created by tim on 03.06.16.
 */
object UtilsRecord {
  def asCalendar(milis: Long): java.util.Calendar = {
    val date = java.util.Calendar.getInstance()
    date.setTimeInMillis(milis)
    date
  }
  def timeWork(f: => Unit):Unit = {
    val start_t = System.currentTimeMillis()
    f
    println(System.currentTimeMillis() - start_t)
  }
}

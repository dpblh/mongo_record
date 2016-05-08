package migration

import java.text.SimpleDateFormat
import scala.collection.JavaConversions._
import org.reflections.Reflections
import org.slf4j.LoggerFactory

/**
 * Created by tim on 08.05.16.
 */
class MigrationRunner(val store: MigrationStore) {

  val logger = LoggerFactory.getLogger(MigrationRunner.getClass)
  val formatter = new SimpleDateFormat("dd-MM-yyyy HH:mm")
  val migrationClasses = new Reflections("migration").getSubTypesOf(classOf[Migration]).toList.map(_.newInstance()).sortBy { a => a.timestamp }

  def run(): Unit = {

    val theMigrationIds = store.migrations()

    migrationClasses.diff(theMigrationIds)
      .foreach(up)

  }

  def next(migration: Option[String] = None): Unit = migration match {
    case Some(id) =>
      val time = formatter.parse(id).getTime
      migrationClasses.filter(_.timestamp <= time).foreach(up)
    case None => if (migrationClasses.nonEmpty) up(migrationClasses.head)
  }

  def prev(migration: Option[String] = None): Unit = migration match {
    case Some(id) =>
      val time = formatter.parse(id).getTime
      store.migrations().filter(_.timestamp >= time).sortWith { (a, b) => a.timestamp > b.timestamp }.foreach(down)
    case None => if (store.migrations().nonEmpty) down(store.migrations().last)
  }


  private def up(migration: Migration) = {
    logger.info("up start => %s".format(migration.className))
    migration.up()
    logger.info("up end => %s".format(migration.className))
    store.save(migration)
  }

  private def down(migration: Migration) = {
    logger.info("down start => %s".format(migration.className))
    Class.forName(migration.className).newInstance().asInstanceOf[Migration].down()
    logger.info("down end => %s".format(migration.className))
    store.remove(migration)
  }

}

object MigrationRunner {
  def apply(store: MigrationStore) = new MigrationRunner(store)
}

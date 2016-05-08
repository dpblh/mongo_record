package migration

import org.scalatest.{FreeSpec, Matchers}
/**
 * Created by tim on 07.05.16.
 */

class SimpleMigrationStore(store: scala.collection.mutable.ArrayBuffer[(Long, String, String)]) extends MigrationStore {
  def migrations() = {
    store.map { e =>
      Class.forName(e._3.asInstanceOf[String]).newInstance().asInstanceOf[Migration]
    }.toList
  }
  def remove(migration: Migration) = {
    store.find(_._1 == migration.timestamp) match {
      case Some(x) =>  store -= x
    }
  }
  def save(migration: Migration) = {
    store += ((migration.timestamp, migration.author, migration.className))
  }
}

object SimpleMigrationStore {
  def apply(store: scala.collection.mutable.ArrayBuffer[(Long, String, String)]) = new SimpleMigrationStore(store)
}

class Migration1 extends Migration {
  override val timestamp: Long = formatter("05-05-2016 12:30")
  override val author: String = "bajurovt@gmail.com"
}

class Migration2 extends Migration {
  override val timestamp: Long = formatter("05-05-2016 12:31")
  override val author: String = "bajurovt@gmail.com"
}

class Migration3 extends Migration {
  override val timestamp: Long = formatter("05-05-2016 12:29")
  override val author: String = "bajurovt@gmail.com"
}

class MigrationTest extends FreeSpec with Matchers {

  val versions = scala.collection.mutable.ArrayBuffer[(Long, String, String)]()

  val migration = MigrationRunner(SimpleMigrationStore(versions))


  versions.clear()

  versions.length shouldBe 0

  migration.run()

  versions.length shouldBe 3

  migration.prev()

  versions.length shouldBe 2

  migration.prev(Some("05-05-2016 12:29"))

  versions.length shouldBe 0

  versions.clear()

}

package migration

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection
import org.scalatest.{FreeSpec, Matchers}
/**
 * Created by tim on 07.05.16.
 */

class MongoMigrationStore(db: MongoDB) extends MigrationStore {
  val versions = db("versions")
  def migrations() = {
    versions.map { e =>
      Class.forName(e("className").asInstanceOf[String]).newInstance().asInstanceOf[Migration]
    }.toList
  }
  def remove(migration: Migration) = {
    versions.remove(Map("_id" -> migration.timestamp))
  }
  def save(migration: Migration) = {
    versions.insert(Map("_id" -> migration.timestamp, "author" -> migration.author, "className" -> migration.className))
  }
}

object MongoMigrationStore {
  def apply(db: MongoDB) = new MongoMigrationStore(db)
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

  val db = MongoConnection()("migration_test")
  val versions = db("versions")

  val migration = MigrationRunner(MongoMigrationStore(db))


  versions.dropCollection()

  versions.count() shouldBe 0

  migration.run()

  versions.count() shouldBe 3

  migration.prev()

  versions.count() shouldBe 2

  migration.prev(Some("05-05-2016 12:29"))

  versions.count() shouldBe 0

  versions.dropCollection()

}

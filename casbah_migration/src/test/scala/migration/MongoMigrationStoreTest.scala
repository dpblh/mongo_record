package migration

import com.mongodb.casbah.MongoConnection
import migration.casbah.MongoMigrationStore
import org.scalatest.{Matchers, FreeSpec}

/**
 * Created by tim on 08.05.16.
 */
class MongoMigrationStoreTest  extends FreeSpec with Matchers {

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
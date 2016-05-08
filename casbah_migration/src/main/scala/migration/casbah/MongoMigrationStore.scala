package migration.casbah

import com.mongodb.casbah.Imports._
import migration.{Migration, MigrationStore}

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
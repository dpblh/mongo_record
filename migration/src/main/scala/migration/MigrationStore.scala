package migration

/**
 * Created by tim on 08.05.16.
 */
trait MigrationStore {
  def migrations(): List[Migration]
  def remove(id: Migration): Unit
  def save(migration: Migration): Unit
}

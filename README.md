### Active record

Как вдохновение взята модель из https://github.com/rails/rails/tree/master/activerecord

Достаточно проаннотировать любую сущность аннотацией `mongoRecord`

`@entityName` необходима для легаси структуры базы

В предикате поддерживаются все стандартные булевские выражения.

Производительность кода на уровне кодогенераторов, т.к. так как весь год генерируется макросами на этапы компиляции.

Вся работа с предикатоми, выборкой безопасна по типам. 
Первый параметр, подаваемый на вход `find` | `modify` обьект, несущий мета информацию о обьекте, к которым была пременена аннотация

Вся магия в обьекте `Field`, который несет информацию о владельце поля и типе поля
```scala
trait Field[C, F] extends Make[C] {
  ...
  def ===[B <: F](right: B)   = BooleanExpression(this, right, "$eq")
  def >(right: F)             = BooleanExpression(this, right, "$gt")
  def <(right: F)             = BooleanExpression(this, right, "$lt")
  def >=(right: F)            = BooleanExpression(this, right, "$gte")
  def <=(right: F)            = BooleanExpression(this, right, "$lte")
  ...
}
```

```scala
@mongoRecord case class Address(region: String, city: String)
@mongoRecord @entityName(name = "person_no_conflict2") case class Person(name: String, @entityName(name = "old") age: Int, address: Address)
```

и на выходе сущность бесплатно получит методы поиска, обновления, вставки и удаления

```scala
Person.find { p =>
  where(p.name === "tim") select p
}.fetch
```

или если нужно вытащить только поля

```scala
Person.find { p =>
  where(p.name === "tim") select(p.name, p.address)
}.fetch
```

или объединяющие условия

```scala
Person.find { p =>
  where(p.name === "tim" && p.age > 28) select p
}.fetchOne
```

или так

```scala
Person.find { p =>
  where(p.age > 18 && p.age < 28) select p
}.fetchOne
```

или дополняющие
```scala
Person.find { p =>
  where(p.name === "tim" || p.name === "ivan") select p
}.fetchOne
```

обновить все
```scala
Person.modify { p =>
  where(p.name === "tim") set(p.age, 29)
}.modify()
```

обновить одну запись
```scala
Person.modify { p =>
  where(p.name === "tim") set(p.age, 29)
}.modifyOne()
```

вставка
```scala
Person("tim", 25, Address("Kalinin", "Tver")).save.flash
```

удаление
```scala
Person.where { p =>
  p.name === "tim"
}.remove
```

количество записей
```scala
Person.where { p =>
  p.name === "tim"
}.count shouldBe 4
```

или более короткая запись без предиката
```scala
Person.where.count
```

или поиск по динамическим полям
```scala
Person.find { p =>
  where(p.dynamic("address") === Map("region" -> "Kalinin", "city" -> "Tver")) select p
}.fetch.length shouldBe 1
```

или более сложная реализация с join

```scala
case class Patient(id: String, snils: String, address_id: String)
case class Address(id: String, street: String)
case class PersonalData(person_id: String, firstName: String, lastName: String)
object Patient extends MongoRecordMacro {

  def patient = meta[Patient]
  def address = meta[Address]
  def personalData = meta[PersonalData]

  def init():Unit = {

    address.where.remove
    address.insert(Address("1", "K")).flash

    patient.where.remove
    patient.insert(Patient("tim", "123", "1")).flash
    patient.insert(Patient("tim", "123", "0")).flash

    personalData.where.remove
    personalData.insert(PersonalData("tim", "tim", "bay")).flash
    personalData.insert(PersonalData("tim", "tim", "bay")).flash
  }

  def allInfo = join(patient, personalData) { (p, pd) =>
    where(p.id === "tim") on(p.id === pd.person_id)
  }

  def hashMany = join(patient, personalData) { (p, pd) =>
    where(p.id === "tim") on(p.id << pd.person_id)
  }

  def triple = join(patient, personalData, address) { (p, pd, ad) =>
    where(p.id === "tim") on(p.id << pd.person_id, p.address_id === ad.id)
  }

}

class GroupTest extends Spec {

  Patient.init()

  Patient.allInfo.fetch.length shouldBe 2

  Patient.hashMany.fetch.length shouldBe 2

  Patient.triple.fetch.length shouldBe 2

  yes(Patient.triple.fetchOne, (Patient("tim","123","1"),List(PersonalData("tim","tim","bay"), PersonalData("tim","tim","bay")),Some(Address("1","K"))))

}
```
package record.macros

import record.Spec
import record.macroses.serializer.DBObjectSerializer._
/**
 * Created by tim on 09.05.16.
 */

case class Address(region: String, city: String)
case class Person(name: String, age: Int, address: Address)

class MacroSerializerTest extends Spec {

  val reader = DBOGenerator[Person]
  yes(reader.asString(Person("tim", 23, Address("Kal", "Tver"))), """{ "name" : "tim" , "age" : 23 , "address" : { "region" : "Kal" , "city" : "Tver"}}""")


}

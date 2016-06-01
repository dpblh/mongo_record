package record

/**
 * Created by tim on 02.06.16.
 */
class A
class AB
class ABC
class Aa
class AaB
class AaBC
class AaBCc
class A_a
class A_aB
object `object1` {
  class A
  class AB
  class ABC
  class Aa
  class AaB
  class AaBC
  class AaBCc
  class A_a
  class A_aB
}
object `object2` {
  object `object` {
    class A
    class AB
    class ABC
    class Aa
    class AaB
    class AaBC
    class AaBCc
    class A_a
    class A_aB
  }
}
class ReflectionRecordTest extends Spec {

  ReflectionRecord.getName(classOf[A]) shouldBe "a"
  ReflectionRecord.getName(classOf[AB]) shouldBe "ab"
  ReflectionRecord.getName(classOf[ABC]) shouldBe "abc"
  ReflectionRecord.getName(classOf[Aa]) shouldBe "aa"
  ReflectionRecord.getName(classOf[AaB]) shouldBe "aa_b"
  ReflectionRecord.getName(classOf[AaBC]) shouldBe "aa_bc"
  ReflectionRecord.getName(classOf[AaBCc]) shouldBe "aa_b_cc"
  ReflectionRecord.getName(classOf[A_a]) shouldBe "a_a"
  ReflectionRecord.getName(classOf[A_aB]) shouldBe "a_a_b"

  ReflectionRecord.getName(classOf[`object1`.A]) shouldBe "a"
  ReflectionRecord.getName(classOf[`object1`.AB]) shouldBe "ab"
  ReflectionRecord.getName(classOf[`object1`.ABC]) shouldBe "abc"
  ReflectionRecord.getName(classOf[`object1`.Aa]) shouldBe "aa"
  ReflectionRecord.getName(classOf[`object1`.AaB]) shouldBe "aa_b"
  ReflectionRecord.getName(classOf[`object1`.AaBC]) shouldBe "aa_bc"
  ReflectionRecord.getName(classOf[`object1`.AaBCc]) shouldBe "aa_b_cc"
  ReflectionRecord.getName(classOf[`object1`.A_a]) shouldBe "a_a"
  ReflectionRecord.getName(classOf[`object1`.A_aB]) shouldBe "a_a_b"

  ReflectionRecord.getName(classOf[`object2`.`object`.A]) shouldBe "a"
  ReflectionRecord.getName(classOf[`object2`.`object`.AB]) shouldBe "ab"
  ReflectionRecord.getName(classOf[`object2`.`object`.ABC]) shouldBe "abc"
  ReflectionRecord.getName(classOf[`object2`.`object`.Aa]) shouldBe "aa"
  ReflectionRecord.getName(classOf[`object2`.`object`.AaB]) shouldBe "aa_b"
  ReflectionRecord.getName(classOf[`object2`.`object`.AaBC]) shouldBe "aa_bc"
  ReflectionRecord.getName(classOf[`object2`.`object`.AaBCc]) shouldBe "aa_b_cc"
  ReflectionRecord.getName(classOf[`object2`.`object`.A_a]) shouldBe "a_a"
  ReflectionRecord.getName(classOf[`object2`.`object`.A_aB]) shouldBe "a_a_b"

}

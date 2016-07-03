package record.reflection

import record.{ReflectionRecord, Spec}

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

  ReflectionRecord.getNameAsUnderscores(classOf[A]) shouldBe "a"
  ReflectionRecord.getNameAsUnderscores(classOf[AB]) shouldBe "ab"
  ReflectionRecord.getNameAsUnderscores(classOf[ABC]) shouldBe "abc"
  ReflectionRecord.getNameAsUnderscores(classOf[Aa]) shouldBe "aa"
  ReflectionRecord.getNameAsUnderscores(classOf[AaB]) shouldBe "aa_b"
  ReflectionRecord.getNameAsUnderscores(classOf[AaBC]) shouldBe "aa_bc"
  ReflectionRecord.getNameAsUnderscores(classOf[AaBCc]) shouldBe "aa_b_cc"
  ReflectionRecord.getNameAsUnderscores(classOf[A_a]) shouldBe "a_a"
  ReflectionRecord.getNameAsUnderscores(classOf[A_aB]) shouldBe "a_a_b"

  ReflectionRecord.getNameAsUnderscores(classOf[`object1`.A]) shouldBe "a"
  ReflectionRecord.getNameAsUnderscores(classOf[`object1`.AB]) shouldBe "ab"
  ReflectionRecord.getNameAsUnderscores(classOf[`object1`.ABC]) shouldBe "abc"
  ReflectionRecord.getNameAsUnderscores(classOf[`object1`.Aa]) shouldBe "aa"
  ReflectionRecord.getNameAsUnderscores(classOf[`object1`.AaB]) shouldBe "aa_b"
  ReflectionRecord.getNameAsUnderscores(classOf[`object1`.AaBC]) shouldBe "aa_bc"
  ReflectionRecord.getNameAsUnderscores(classOf[`object1`.AaBCc]) shouldBe "aa_b_cc"
  ReflectionRecord.getNameAsUnderscores(classOf[`object1`.A_a]) shouldBe "a_a"
  ReflectionRecord.getNameAsUnderscores(classOf[`object1`.A_aB]) shouldBe "a_a_b"

  ReflectionRecord.getNameAsUnderscores(classOf[`object2`.`object`.A]) shouldBe "a"
  ReflectionRecord.getNameAsUnderscores(classOf[`object2`.`object`.AB]) shouldBe "ab"
  ReflectionRecord.getNameAsUnderscores(classOf[`object2`.`object`.ABC]) shouldBe "abc"
  ReflectionRecord.getNameAsUnderscores(classOf[`object2`.`object`.Aa]) shouldBe "aa"
  ReflectionRecord.getNameAsUnderscores(classOf[`object2`.`object`.AaB]) shouldBe "aa_b"
  ReflectionRecord.getNameAsUnderscores(classOf[`object2`.`object`.AaBC]) shouldBe "aa_bc"
  ReflectionRecord.getNameAsUnderscores(classOf[`object2`.`object`.AaBCc]) shouldBe "aa_b_cc"
  ReflectionRecord.getNameAsUnderscores(classOf[`object2`.`object`.A_a]) shouldBe "a_a"
  ReflectionRecord.getNameAsUnderscores(classOf[`object2`.`object`.A_aB]) shouldBe "a_a_b"

}

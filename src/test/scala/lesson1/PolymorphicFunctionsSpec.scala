package lesson1

import org.scalatest.{MustMatchers, WordSpec}

class PolymorphicFunctionsSpec extends WordSpec with MustMatchers {

  import lesson1.PolymorphicFuncs._

  "Polymorphic functions spec" must {
    "create list of duplicates" in {
      val cat = Cat("Garfield")

      listOfDuplicates(3, 5) mustBe List(3, 3, 3, 3, 3)
      listOfDuplicates("AAA", 2) mustBe List("AAA", "AAA")
      listOfDuplicates(cat, 3) mustBe List(cat, cat, cat)
    }
  }

}

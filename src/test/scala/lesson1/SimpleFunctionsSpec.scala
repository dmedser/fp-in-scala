package lesson1

import org.scalatest.{MustMatchers, WordSpec}

class SimpleFunctionsSpec extends WordSpec with MustMatchers {

  "SimpleFunctionsSpec" must {
    "make simple functions just work" in {
      val double: Int => Int = x => x * 2
      double(2) mustBe 4

      val addOne: Int => Int = x => x + 1
      addOne(1) mustBe 2

      val intToString: Int => String = x => s"Result is: ${x.toString}"

      val func = double andThen addOne andThen intToString
      func(10) mustBe "Result is: 21"
    }
  }

}

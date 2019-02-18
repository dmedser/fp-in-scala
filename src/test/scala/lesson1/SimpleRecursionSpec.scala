package lesson1

import org.scalatest.{MustMatchers, WordSpec}

class SimpleRecursionSpec extends WordSpec with MustMatchers {

  import lesson1.SimpleTailRec._

  "SimpleRecursionSpec" must {
    "blow stack with no tail optimization" in {
      an [StackOverflowError] should be thrownBy unsafeFib(100500)
    }

    "safely call tail recursion" in {
      safeFib(100500) mustBe 386213401
    }
  }

}
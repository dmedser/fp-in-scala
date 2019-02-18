package lesson1

import org.scalatest.{MustMatchers, WordSpec}

class RefUntransparencySpec extends WordSpec with MustMatchers {

  "RefUntransparencySpec" must {
    "broke referential transparency for unpure functions" in {
      var total = 0

      def addToTotal(x: Int): Int = {
        total += x
        total
      }

      addToTotal(1) == addToTotal(1) mustBe false
    }
  }

}

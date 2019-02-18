package lesson1

import org.scalatest.{MustMatchers, WordSpec}

class RefTransparencySpec extends WordSpec with MustMatchers {

  "RefTransparencySpec" must {
    "prove function referential transparency" in {
      val area = (radius: Int) => math.Pi * math.pow(radius, 2)
      val res1 = area(3) + area(4)
      val res2 = math.Pi * math.pow(3, 2) + math.Pi * math.pow(4, 2)

      res1 mustBe res2
    }
  }

}

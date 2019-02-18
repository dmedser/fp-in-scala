package lesson3

import org.scalatest.{MustMatchers, WordSpec}

class MaybeLaws extends WordSpec with MustMatchers {

  "Maybe" must {
    val v = 1
    val m = Maybe(v)
    val f: Int => Maybe[String] = x => Maybe(x.toString)
    val g: String => Maybe[Double] = x => Maybe(x.toDouble)

    "follow left identity law" in {
      m.flatMap(f) mustBe f(v)
    }

    "follow right identity law" in {
      m.flatMap(Maybe(_)) mustBe m
    }

    "follow associativity law" in {
      m.flatMap(f).flatMap(g) mustBe m.flatMap(x => f(x).flatMap(g))
    }
  }

}

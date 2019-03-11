package lesson2

import org.scalatest.{MustMatchers, WordSpec}

class MyLinkedListSpec extends WordSpec with MustMatchers {

  val list = MyLinkedList(1, 2, 3, 4)

  "MyLinkedListSpec" must {

    "map" in {
      list.map(_ + 1) mustBe MyLinkedList(2, 3, 4, 5)
    }

    "append" in {
      val l1 = MyLinkedList(1, 2)
      val l2 = MyLinkedList(3, 4)

      l1.append(l2) mustBe list
      l1 +++ l2 mustBe list
    }

    "drop" in {
      list.drop(2) mustBe MyLinkedList(3, 4)
    }

    "dropWhile" in {
      list.dropWhile(_ < 3) mustBe MyLinkedList(3, 4)
    }

    "foldLeft" in {
      list.foldLeft(0)(_ - _) mustBe -10
    }

    "foldRight" in {
      list.foldRight(0)(_ - _) mustBe -2
    }

    "init" in {
      list.init mustBe MyLinkedList(1, 2, 3)
    }

    "reverse" in {
      list.reverse mustBe MyLinkedList(4, 3, 2, 1)
    }

    "length" in  {
      list.length mustBe 4
    }

    "flatten" in {
      val listOfLists = MyLinkedList(MyLinkedList(1, 2), MyLinkedList(3, 4), MyLinkedList(5, 6))
      listOfLists.flatten mustBe MyLinkedList(1, 2, 3, 4, 5, 6)
    }

    "flatMap" in {
      list.flatMap(x ⇒ MyLinkedList(x, x)) mustBe MyLinkedList(1, 1, 2, 2, 3, 3, 4, 4)
    }

  }

}
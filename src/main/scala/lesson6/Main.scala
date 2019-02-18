package lesson6

import cats.Monoid
import cats.kernel.Semigroup
//import cats.instances.int._
import lesson6.instances.map._

case class Person(name: String, age: Int)

object Main extends App {

/*  val mapStrIntMonoid = Monoid[Map[String, Int]]

  val mapStrPersonMonoid = Monoid[Map[String, Person]]

  val m1 = Map("a" → 1, "b" → 2)

  val m2 = Map("c" → 3, "d" → 4)

  val m3 = Map("a" → 1, "e" → 5)



  println(mapStrIntMonoid.combine(m1, m2) == mapStrIntMonoid.combine(m2, m1))
  println(mapStrIntMonoid.combine(m1, m1) == m1)*/

  def foldr[A, B](fa: List[A])(z: B)(f: (A, B) => B): B = {
    fa.foldLeft(z)((b: B, a: A) ⇒ f(a, b))
  }



  println(foldr(List(1, 2, 3))(0)(_ - _) == List(1, 2, 3).foldRight(0)(_ - _))


  println(List.empty[Int].foldRight(Option.empty[Int]) { case (a, b) ⇒ Some(a) })

}

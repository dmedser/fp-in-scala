package lesson13

import cats.data.State

object CountAndSumPositiveNumbers extends App {

  import cats.Monoid
  import cats.instances.int._
  import cats.syntax.semigroup._

  def countByPredicate[A : Monoid](list: List[A], p: A => Boolean): State[Int, A] =
    list match {
      case x :: xs if p(x) =>
        for {
          _ <- State.modify[Int](_ + 1)
          restAppropriateXs <- countByPredicate(xs, p)
        } yield x |+| restAppropriateXs
      case x :: xs if !p(x) =>
        countByPredicate(xs, p)
      case Nil =>
        State.empty
    }

  val numbers = List(-1, -2, 0, 1, 2, -3, 4, 0, 0, 10)

  println(countByPredicate[Int](numbers, _ > 0).run(0).value)
}
package lesson14.laws

import cats.laws._
import lesson14.Monad

trait OptionMonadLaws {
  implicit def F: Monad[Option]

  def monadLeftIdentity[A, B](a: A, f: A => Option[B]): IsEq[Option[B]] =
    F.pure(a).flatMap(f) <-> f(a)

  def monadRightIdentity[A](option: Option[A]): IsEq[Option[A]] =
    option.flatMap(F.pure) <-> option

  def mapFlatMapCoherence[A, B](option: Option[A], f: A => B): IsEq[Option[B]] =
    option.flatMap(a => F.pure(f(a))) <-> option.map(f)
}

object OptionMonadLaws {
  def apply(implicit ev: Monad[Option]): OptionMonadLaws =
    new OptionMonadLaws { def F: Monad[Option] = ev }
}

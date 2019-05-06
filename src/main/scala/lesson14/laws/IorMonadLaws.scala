package lesson14.laws

import cats.laws.IsEq
import lesson14.{Ior, Monad}
import cats.laws._

trait IorMonadLaws[E] {
  implicit def F: Monad[Ior[E, ?]]

  def monadLeftIdentity[A, B](a: A, f: A => Ior[E, B]): IsEq[Ior[E, B]] =
    F.flatMap(F.pure(a))(f) <-> f(a)

  def monadRightIdentity[A](ior: Ior[E, A]): IsEq[Ior[E, A]] =
    F.flatMap(ior)(F.pure) <-> ior

  def mapFlatMapCoherence[A, B](ior: Ior[E, A], f: A => B): IsEq[Ior[E, B]] =
    F.flatMap(ior)(a => F.pure(f(a))) <-> F.map(ior)(f)
}

object IorMonadLaws {
  def apply[E](implicit ev: Monad[Ior[E, ?]]): IorMonadLaws[E] = new IorMonadLaws[E] {
    def F: Monad[Ior[E, ?]] = ev
  }
}
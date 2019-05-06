package lesson14.laws

import cats.laws.IsEq
import cats.laws._
import lesson14.{FTree, Monad}

trait FTreeMonadLaws[F[_]] {
  implicit def F: Monad[FTree[F, ?]]

  def monadLeftIdentity[A, B](a: A, f: A => FTree[F, B]): IsEq[FTree[F, B]] =
    F.flatMap(F.pure(a))(f) <-> f(a)

  def monadRightIdentity[A](ftree: FTree[F, A]): IsEq[FTree[F, A]] =
    F.flatMap(ftree)(F.pure) <-> ftree

  def mapFlatMapCoherence[A, B](ftree: FTree[F, A], f: A => B): IsEq[FTree[F, B]] =
    F.flatMap(ftree)(a => F.pure(f(a))) <-> F.map(ftree)(f)
}

object FTreeMonadLaws {
  def apply[F[_]](implicit ev: Monad[FTree[F, ?]]): FTreeMonadLaws[F] = new FTreeMonadLaws[F] {
    def F: Monad[FTree[F, ?]] = ev
  }
}

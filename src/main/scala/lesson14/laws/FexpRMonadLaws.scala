package lesson14.laws

import cats.laws._
import lesson14.Monad

trait FexpRMonadLaws[R, F[_]] {
  implicit def F: Monad[F]
  implicit def G: Monad[λ[α => R => F[α]]]

  def monadLeftIdentity[A, B](a: A, f: A => R => F[B]): IsEq[R => F[B]] =
    G.flatMap(G.pure(a))(f) <-> f(a)

  def monadRightIdentity[A](rToFa: R => F[A]): IsEq[R => F[A]] =
    G.flatMap(rToFa)(G.pure) <-> rToFa

  def mapFlatMapCoherence[A, B](rToFa: R => F[A], f: A => B): IsEq[R => F[B]] =
    G.flatMap(rToFa)(a => G.pure(f(a))) <-> G.map(rToFa)(f)
}

object FexpRMonadLaws {
  def apply[R, F[_]](implicit f: Monad[F], g: Monad[λ[α => R => F[α]]]): FexpRMonadLaws[R, F] =
    new FexpRMonadLaws[R, F] { def F: Monad[F] = f; def G: Monad[λ[α => R => F[α]]] = g }
}

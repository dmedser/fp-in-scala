package lesson14.laws

import cats.laws._
import lesson14.Monad
import lesson14.`package`.:*:

trait AandFMonadLaws[F[_]] {
  implicit def F: Monad[F]
  implicit def G: Monad[λ[α => α :*: F[α]]]

  def monadLeftIdentity[A, B](a: A, f: A => B :*: F[B]): IsEq[B :*: F[B]] =
    G.flatMap(G.pure(a))(f) <-> f(a)

  def monadRightIdentity[A](aAndF: A :*: F[A]): IsEq[A :*: F[A]] =
    G.flatMap(aAndF)(G.pure) <-> aAndF

  def mapFlatMapCoherence[A, B](aAndF: A :*: F[A], f: A => B): IsEq[B :*: F[B]] =
    G.flatMap(aAndF)(a => G.pure(f(a))) <-> G.map(aAndF)(f)
}

object AandFMonadLaws {
  def apply[F[_]](implicit f: Monad[F], g: Monad[λ[α => α :*: F[α]]]): AandFMonadLaws[F] =
    new AandFMonadLaws[F] { def F: Monad[F] = f; def G: Monad[λ[α => α :*: F[α]]] = g }
}

package lesson14.laws

import cats.laws.{IsEq, _}
import lesson14.Monad
import lesson14.`package`.:*:

trait Prod2MonadsLaws[F[_], G[_]] {
  implicit def F: Monad[F]
  implicit def G: Monad[G]
  implicit def H: Monad[λ[α => F[α] :*: G[α]]]

  def monadLeftIdentity[A, B](a: A, f: A => F[B] :*: G[B]): IsEq[F[B] :*: G[B]] =
    H.flatMap(H.pure(a))(f) <-> f(a)

  def monadRightIdentity[A](ForG: F[A] :*: G[A]): IsEq[F[A] :*: G[A]] =
    H.flatMap(ForG)(H.pure) <-> ForG

  def mapFlatMapCoherence[A, B](ForG: F[A] :*: G[A], f: A => B): IsEq[F[B] :*: G[B]] =
    H.flatMap(ForG)(a => H.pure(f(a))) <-> H.map(ForG)(f)
}

object Prod2MonadsLaws {
  def apply[F[_], G[_]](implicit f: Monad[F], g: Monad[G], h: Monad[λ[α => F[α] :*: G[α]]]): Prod2MonadsLaws[F, G] =
    new Prod2MonadsLaws[F, G] { def F: Monad[F] = f; def G: Monad[G] = g; def H: Monad[λ[α => F[α] :*: G[α]]] = h }
}

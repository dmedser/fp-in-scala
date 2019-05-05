package lesson14.laws

import cats.laws._
import lesson10.Applikative
import lesson11.Traversable
import lesson14.Monad

trait ComposeMonadsLaws[F[_], G[_]] {
  implicit def MF: Monad[F]
  implicit def TF: Traversable[F]
  implicit def AF: Applikative[F]
  implicit def MG: Monad[G]
  implicit def H: Monad[λ[α => F[G[α]]]]

  def monadLeftIdentity[A, B](a: A, f: A => F[G[B]]): IsEq[F[G[B]]] =
    H.flatMap(H.pure(a))(f) <-> f(a)

  def monadRightIdentity[A](GinF: F[G[A]]): IsEq[F[G[A]]] =
    H.flatMap(GinF)(H.pure) <-> GinF

  def mapFlatMapCoherence[A, B](GinF: F[G[A]], f: A => B): IsEq[F[G[B]]] =
    H.flatMap(GinF)(a => H.pure(f(a))) <-> H.map(GinF)(f)
}

object ComposeMonadsLaws {
  def apply[F[_], G[_]](
    implicit
    mf: Monad[F],
    tf: Traversable[F],
    af: Applikative[F],
    mg: Monad[G],
    h: Monad[λ[α => F[G[α]]]]
  ): ComposeMonadsLaws[F, G] =
    new ComposeMonadsLaws[F, G] {
      def MF: Monad[F] = mf
      def TF: Traversable[F] = tf
      def AF: Applikative[F] = af
      def MG: Monad[G] = mg
      def H: Monad[λ[α => F[G[α]]]] = h
    }
}

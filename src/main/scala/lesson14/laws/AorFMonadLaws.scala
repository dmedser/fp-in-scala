package lesson14.laws

import cats.laws.{IsEq, _}
import lesson14.Monad
import lesson14.`package`.:+:

trait AorFMonadLaws[F[_]] {
  implicit def F: Monad[F]
  implicit def G: Monad[λ[α => α :+: F[α]]]

  def monadLeftIdentity[A, B](a: A, f: A => B :+: F[B]): IsEq[B :+: F[B]] =
    G.flatMap(G.pure(a))(f) <-> f(a)

  def monadRightIdentity[A](aOrF: A :+: F[A]): IsEq[A :+: F[A]] =
    G.flatMap(aOrF)(G.pure) <-> aOrF

  def mapFlatMapCoherence[A, B](aOrF: A :+: F[A], f: A => B): IsEq[B :+: F[B]] =
    G.flatMap(aOrF)(a => G.pure(f(a))) <-> G.map(aOrF)(f)
}

object AorFMonadLaws {
  def apply[F[_]](implicit f: Monad[F], g: Monad[λ[α => α :+: F[α]]]): AorFMonadLaws[F] =
    new AorFMonadLaws[F] { def F: Monad[F] = f; def G: Monad[λ[α => α :+: F[α]]] = g }
}

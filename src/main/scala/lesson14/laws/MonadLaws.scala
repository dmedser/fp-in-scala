package lesson14.laws

import cats.laws.{IsEq, _}
import lesson14.Monad
import lesson14.`package`.{:*:, :+:}

trait MonadLaws {

  /* Either */

  def eitherMonadLeftIdentity[A, B, C](b: B, f: B => A :+: C)(
    implicit F: Monad[Either[A, ?]]
  ): IsEq[A :+: C] =
    F.flatMap(F.pure(b))(f) <-> f(b)

  def eitherMonadRightIdentity[A, B](aOrB: A :+: B)(
    implicit F: Monad[Either[A, ?]]
  ): IsEq[A :+: B] =
    F.flatMap(aOrB)(F.pure) <-> aOrB

  def eitherMapFlatMapCoherence[A, B, C](aOrB: A :+: B, f: B => C)(
    implicit F: Monad[Either[A, ?]]
  ): IsEq[A :+: C] =
    F.flatMap(aOrB)(b => F.pure(f(b))) <-> F.map(aOrB)(f)



  /* aAndF */

  def aAndFMonadLeftIdentity[F[_] : Monad, A, B](a: A, f: A => B :*: F[B])(
    implicit F: Monad[λ[α => α :*: F[α]]]
  ): IsEq[B :*: F[B]] =
    F.flatMap(F.pure(a))(f) <-> f(a)

  def aAndFMonadRightIdentity[F[_] : Monad, A](aAndF: A :*: F[A])(
    implicit F: Monad[λ[α => α :*: F[α]]]
  ): IsEq[A :*: F[A]] =
    F.flatMap(aAndF)(F.pure) <-> aAndF

  def aAndFMapFlatMapCoherence[F[_] : Monad, A, B](aAndF: A :*: F[A], f: A => B)(
    implicit F: Monad[λ[α => α :*: F[α]]]
  ): IsEq[B :*: F[B]] =
    F.flatMap(aAndF)(a => F.pure(f(a))) <-> F.map(aAndF)(f)



  /* aOrF */

  def aOrFMonadLeftIdentity[F[_] : Monad, A, B](a: A, f: A => B :+: F[B])(
    implicit F: Monad[λ[α => α :+: F[α]]]
  ): IsEq[B :+: F[B]] =
    F.flatMap(F.pure(a))(f) <-> f(a)

  def aOrFMonadRightIdentity[F[_] : Monad, A](aOrF: A :+: F[A])(
    implicit F: Monad[λ[α => α :+: F[α]]]
  ): IsEq[A :+: F[A]] =
    F.flatMap(aOrF)(F.pure) <-> aOrF

  def aOrFMapFlatMapCoherence[F[_] : Monad, A, B](aOrF: A :+: F[A], f: A => B)(
    implicit F: Monad[λ[α => α :+: F[α]]]
  ): IsEq[B :+: F[B]] =
    F.flatMap(aOrF)(a => F.pure(f(a))) <-> F.map(aOrF)(f)
}

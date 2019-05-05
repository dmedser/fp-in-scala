package lesson14.laws

import cats.laws._
import lesson14.Monad
import lesson14.`package`.:+:

trait EitherMonadLaws[A] {

  implicit def F: Monad[Either[A, ?]]

  def monadLeftIdentity[B, C](b: B, f: B => A :+: C): IsEq[A :+: C] =
    F.flatMap(F.pure(b))(f) <-> f(b)

  def monadRightIdentity[B](aOrB: A :+: B): IsEq[A :+: B] =
    F.flatMap(aOrB)(F.pure) <-> aOrB

  def mapFlatMapCoherence[B, C](aOrB: A :+: B, f: B => C): IsEq[A :+: C] =
    F.flatMap(aOrB)(b => F.pure(f(b))) <-> F.map(aOrB)(f)
}

object EitherMonadLaws {
  def apply[A](implicit ev: Monad[Either[A, ?]]): EitherMonadLaws[A] =
    new EitherMonadLaws[A] { def F: Monad[Either[A, ?]] = ev }
}

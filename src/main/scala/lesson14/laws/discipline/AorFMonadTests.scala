package lesson14.laws.discipline

import cats.Eq
import cats.laws.discipline._
import lesson14.Monad
import lesson14.`package`.:+:
import lesson14.laws.AorFMonadLaws
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait AorFMonadTests[F[_]] extends Laws {
  def laws: AorFMonadLaws[F]

  def monad[A : Arbitrary, B](
    implicit
    ArbAtoB: Arbitrary[A => B],
    ArbAorFA: Arbitrary[A :+: F[A]],
    ArbAtoBorFB: Arbitrary[A => B :+: F[B]],
    EqAorFA: Eq[A :+: F[A]],
    EqBorFB: Eq[B :+: F[B]]
  ): RuleSet =
    new RuleSet {
      def name: String = "AorF monad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Nil
      def props: Seq[(String, Prop)] =
        Seq(
          "monad left identity" -> forAll(laws.monadLeftIdentity[A, B] _),
          "monad right identity" -> forAll(laws.monadRightIdentity[A] _),
          "map flatMap coherence" -> forAll(laws.mapFlatMapCoherence[A, B] _)
        )
    }
}

object AorFMonadTests {
  def apply[F[_]](implicit F: Monad[F], G: Monad[λ[α => α :+: F[α]]]): AorFMonadTests[F] =
    new AorFMonadTests[F] {
      def laws: AorFMonadLaws[F] = AorFMonadLaws[F]
    }
}

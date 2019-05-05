package lesson14.laws.discipline

import cats.Eq
import cats.laws.discipline._
import lesson14.Monad
import lesson14.`package`.:*:
import lesson14.laws.AandFMonadLaws
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait AandFMonadTests[F[_]] extends Laws {
  def laws: AandFMonadLaws[F]

  def monad[A : Arbitrary, B](
    implicit
    ArbAtoB: Arbitrary[A => B],
    ArbAandFA: Arbitrary[A :*: F[A]],
    ArbAtoBandFB: Arbitrary[A => B :*: F[B]],
    EqAandFA: Eq[A :*: F[A]],
    EqBandFB: Eq[B :*: F[B]]
  ): RuleSet =
    new RuleSet {
      def name: String = "AandF monad"
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

object AandFMonadTests {
  def apply[F[_]](implicit F: Monad[F], G: Monad[λ[α => α :*: F[α]]]): AandFMonadTests[F] =
    new AandFMonadTests[F] {
      def laws: AandFMonadLaws[F] = AandFMonadLaws[F]
    }
}

package lesson14.laws.discipline

import cats.Eq
import cats.laws.discipline._
import lesson14.Monad
import lesson14.`package`.:+:
import lesson14.laws.EitherMonadLaws
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.typelevel.discipline.Laws

trait EitherMonadTests[A] extends Laws {
  def laws: EitherMonadLaws[A]

  def monad[B : Arbitrary, C : Arbitrary](
    implicit
    CogenB: Cogen[B],
    ArbAorB: Arbitrary[A :+: B],
    ArbAorC: Arbitrary[A :+: C],
    EqAorB: Eq[A :+: B],
    EqAorC: Eq[A :+: C]
  ): RuleSet =
    new RuleSet {
      def name: String = "Either monad"
      def bases: Seq[(String, RuleSet)] = Nil
      def parents: Seq[RuleSet] = Nil
      def props: Seq[(String, Prop)] =
        Seq(
          "monad left identity" -> forAll(laws.monadLeftIdentity[B, C] _),
          "monad right identity" -> forAll(laws.monadRightIdentity[B] _),
          "map flatMap coherence" -> forAll(laws.mapFlatMapCoherence[B, C] _)
        )
    }
}

object EitherMonadTests {
  def apply[A](implicit F: Monad[Either[A, ?]]): EitherMonadTests[A] =
    new EitherMonadTests[A] {
      def laws: EitherMonadLaws[A] = EitherMonadLaws[A]
    }
}

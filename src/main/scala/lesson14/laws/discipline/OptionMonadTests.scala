package lesson14.laws.discipline

import cats.Eq
import cats.laws.discipline._
import lesson14.Monad
import lesson14.laws.OptionMonadLaws
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait OptionMonadTests extends Laws {
  def laws: OptionMonadLaws

  def monad[A : Arbitrary, B](
    implicit
    ArbAtoB: Arbitrary[A => B],
    ArbAtoFB: Arbitrary[A => Option[B]],
    EqFA: Eq[Option[A]],
    EqFB: Eq[Option[B]]
  ): RuleSet =
    new RuleSet {
      def name: String = "Option monad"
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

object OptionMonadTests {
  def apply(implicit ev: Monad[Option]): OptionMonadTests =
    new OptionMonadTests {
      def laws: OptionMonadLaws = OptionMonadLaws.apply
    }
}

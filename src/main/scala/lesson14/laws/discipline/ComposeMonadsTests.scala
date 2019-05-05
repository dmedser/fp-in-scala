package lesson14.laws.discipline

import cats.Eq
import cats.laws.discipline._
import lesson10.Applikative
import lesson14.Monad
import lesson14.laws.ComposeMonadsLaws
import lesson11.Traversable
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait ComposeMonadsTests[F[_], G[_]] extends Laws {
  def laws: ComposeMonadsLaws[F, G]

  def monad[A : Arbitrary, B](
    implicit
    ArbFGA: Arbitrary[F[G[A]]],
    ArbAtoB: Arbitrary[A => B],
    ArbAtoFGB: Arbitrary[A => F[G[B]]],
    EqFGA: Eq[F[G[A]]],
    EqFGB: Eq[F[G[B]]]
  ): RuleSet =
    new RuleSet {
      def name: String = "ComposeMonads monad"
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

object ComposeMonadsTests {
  def apply[F[_], G[_]](
    implicit
    MF: Monad[F],
    TF: Traversable[F],
    AF: Applikative[F],
    MG: Monad[G],
    H: Monad[λ[α => F[G[α]]]]
  ): ComposeMonadsTests[F, G] =
    new ComposeMonadsTests[F, G] {
      def laws: ComposeMonadsLaws[F, G] = ComposeMonadsLaws[F, G]
    }
}

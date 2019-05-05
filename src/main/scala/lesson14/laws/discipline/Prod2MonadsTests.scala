package lesson14.laws.discipline

import cats.Eq
import cats.laws.discipline._
import lesson14.Monad
import lesson14.`package`.:*:
import lesson14.laws.Prod2MonadsLaws
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait Prod2MonadsTests[F[_], G[_]] extends Laws {
  def laws: Prod2MonadsLaws[F, G]

  def monad[A : Arbitrary, B](
    implicit
    ArbAtoB: Arbitrary[A => B],
    ArbFAandGA: Arbitrary[F[A] :*: G[A]],
    ArbAtoFBandGB: Arbitrary[A => F[B] :*: G[B]],
    EqFAandGA: Eq[F[A] :*: G[A]],
    EqFBandGB: Eq[F[B] :*: G[B]]
  ): RuleSet =
    new RuleSet {
      def name: String = "Prod2Monads monad"
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

object Prod2MonadsTests {
  def apply[F[_], G[_]](implicit F: Monad[F], G: Monad[G], H: Monad[λ[α => F[α] :*: G[α]]]): Prod2MonadsTests[F, G] =
    new Prod2MonadsTests[F, G] {
      def laws: Prod2MonadsLaws[F, G] = Prod2MonadsLaws[F, G]
    }
}

package lesson14.laws.discipline

import cats.Eq
import cats.laws.discipline._
import lesson14.Monad
import lesson14.laws.FexpRMonadLaws
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait FexpRMonadTests[R, F[_]] extends Laws {
  def laws: FexpRMonadLaws[R, F]

  def monad[A : Arbitrary, B](
    implicit
    ArbAtoRtoFB: Arbitrary[A => R => F[B]],
    ArbAtoB: Arbitrary[A => B],
    ArbRtoFA: Arbitrary[R => F[A]],
    EqRtoFB: Eq[R => F[B]],
    EqRroFA: Eq[R => F[A]]
  ): RuleSet =
    new RuleSet {
      def name: String = "FexpR monad"
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

object FexpRMonadTests {
  def apply[R, F[_]](implicit F: Monad[F], G: Monad[λ[α => R => F[α]]]): FexpRMonadTests[R, F] =
    new FexpRMonadTests[R, F] {
      def laws: FexpRMonadLaws[R, F] = FexpRMonadLaws[R, F]
    }
}

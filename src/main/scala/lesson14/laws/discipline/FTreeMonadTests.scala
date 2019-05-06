package lesson14.laws.discipline

import cats.Eq
import lesson14.laws.FTreeMonadLaws
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws
import cats.laws.discipline._
import lesson14.{FTree, Monad}

trait FTreeMonadTests[F[_]] extends Laws {
  def laws: FTreeMonadLaws[F]

  def monad[A : Arbitrary, B](
    implicit
    ArbAtoB: Arbitrary[A => B],
    ArbFTreeFA: Arbitrary[FTree[F, A]],
    ArbAtoFTreeFB: Arbitrary[A => FTree[F, B]],
    EqFTreeFA: Eq[FTree[F, A]],
    EqFTreeFB: Eq[FTree[F, B]]
  ): RuleSet =
    new RuleSet {
      def name: String = "FTree monad"
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

object FTreeMonadTests {
  def apply[F[_]](implicit ev: Monad[FTree[F, ?]]): FTreeMonadTests[F] = new FTreeMonadTests[F] {
    def laws: FTreeMonadLaws[F] = FTreeMonadLaws[F]
  }
}

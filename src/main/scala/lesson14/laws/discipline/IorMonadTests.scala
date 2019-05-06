package lesson14.laws.discipline

import cats.Eq
import cats.laws.discipline._
import lesson14.laws.IorMonadLaws
import lesson14.{Ior, Monad}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws

trait IorMonadTests[E] extends Laws {
  def laws: IorMonadLaws[E]

  def monad[A : Arbitrary, B](
    implicit
    ArbAtoIorEB: Arbitrary[A => Ior[E, B]],
    ArbIorEA: Arbitrary[Ior[E, A]],
    ArbAtoB: Arbitrary[A => B],
    EqIorEA: Eq[Ior[E, A]],
    EqIorEB: Eq[Ior[E, B]]
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

object IorMonadTests {
  def apply[E](implicit ev: Monad[Ior[E, ?]]): IorMonadTests[E] = new IorMonadTests[E] {
    def laws: IorMonadLaws[E] = IorMonadLaws[E]
  }
}

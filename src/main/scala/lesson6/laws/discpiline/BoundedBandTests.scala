package lesson6.laws.discpiline

import cats.Eq
import cats.kernel.laws.discipline.{BandTests, MonoidTests}
import lesson6.BoundedBand
import lesson6.laws.BoundedBandLaws
import org.scalacheck.{Arbitrary, Prop}

trait BoundedBandTests[A] extends BandTests[A] with MonoidTests[A] {

  def laws: BoundedBandLaws[A]

  def boundedBand(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new RuleSet {
      val name: String = "boundedBand"
      val bases: Seq[(String, RuleSet)] = Nil
      val parents: Seq[RuleSet] = Seq(monoid, band)
      val props: Seq[(String, Prop)] = Nil
    }

}

object BoundedBandTests {
  def apply[A: BoundedBand]: BoundedBandTests[A] =
    new BoundedBandTests[A] { def laws: BoundedBandLaws[A] = BoundedBandLaws[A] }
}
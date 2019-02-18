package lesson6

import cats.kernel.Band
import cats.tests.CatsSuite
import cats.kernel.laws.discipline.{BoundedSemilatticeTests, CommutativeMonoidTests, MonoidTests}
import org.scalacheck.Arbitrary.arbContainer
import lesson6.instances.map._
import lesson6.laws.discpiline.BoundedBandTests

class MapSuite extends CatsSuite {

  implicit val band: Band[Int] = _ max _

  checkAll("Map.MonoidLaws", MonoidTests[Map[String, Char]].monoid)
  checkAll("Map.CommutativeLaws", CommutativeMonoidTests[Map[String, Int]].commutativeMonoid)
  checkAll("Map.BoundedBandLaws", BoundedBandTests[Map[String, Int]].boundedBand)
  checkAll("Map.BoundedSemilatticeLaws", BoundedSemilatticeTests[Map[String, Set[Int]]].boundedSemilattice)

}

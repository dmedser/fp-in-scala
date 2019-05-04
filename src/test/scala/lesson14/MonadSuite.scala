package lesson14

import cats.tests.CatsSuite
import lesson14.UtilMonadInstances._
import lesson14.laws.discipline.{AandFMonadTests, AorFMonadTests, EitherMonadTests}
import org.scalacheck.Arbitrary.arbContainer

class MonadSuite extends CatsSuite {

  checkAll("EitherMonadLaws", EitherMonadTests[Int].monad[Double, String])
  checkAll("AandFMonadLaws", AandFMonadTests[List].monad[Int, Double])
  checkAll("AorFMonadLaws", AorFMonadTests[List].monad[Int, Double])

}

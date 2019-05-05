package lesson14

import cats.tests.CatsSuite
import lesson14.UtilMonadInstances._
import lesson11.Traversable._
import lesson14.laws.discipline.{AandFMonadTests, AorFMonadTests, ComposeMonadsTests, EitherMonadTests, OptionMonadTests, Prod2MonadsTests}
import org.scalacheck.Arbitrary.arbContainer

class MonadSuite extends CatsSuite {

  checkAll("EitherMonadLaws", EitherMonadTests[Int].monad[Double, String])
  checkAll("OptionMonadLaws", OptionMonadTests.apply.monad[Boolean, Float])
  checkAll("AandFMonadLaws", AandFMonadTests[Either[Float, ?]].monad[Int, String])
  checkAll("AorFMonadLaws", AorFMonadTests[Option].monad[Double, Long])
  checkAll("Prod2MonadsLaws", Prod2MonadsTests[Option, List].monad[Int, Boolean])
  checkAll("ComposeMonadsLaws", ComposeMonadsTests[List, Option].monad[String, Double])
}

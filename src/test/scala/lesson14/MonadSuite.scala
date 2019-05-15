package lesson14

import cats.tests.CatsSuite
import lesson11.Traversable._
import lesson14.UtilMonadInstances._
import lesson14.laws.discipline._
import org.scalacheck.Arbitrary.arbContainer


class MonadSuite extends CatsSuite {

  checkAll("EitherMonadLaws", EitherMonadTests[Int].monad[Double, String])
  checkAll("OptionMonadLaws", OptionMonadTests.apply.monad[Boolean, Float])
  checkAll("AandFMonadLaws", AandFMonadTests[Either[Float, ?]].monad[Int, String])
  checkAll("AorFMonadLaws", AorFMonadTests[Option].monad[Double, Long])
  checkAll("Prod2MonadsLaws", Prod2MonadsTests[List, Either[Long, ?]].monad[Int, Boolean])
  checkAll("ComposeMonadLaws", ComposeMonadTests[List, List].monad[String, Double])
  checkAll("IorMonadLaws", IorMonadTests[String].monad[Int, Boolean])

}

package lesson3

import monix.eval.Task
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{MustMatchers, WordSpec}

import scala.concurrent.Future
import scala.language.higherKinds
import scala.util.{Success, Try}

class MonadLawsSpec extends WordSpec with MustMatchers with ScalaFutures {

  import Laws._

  "Option" must {
    import cats.instances.option._

    val v = 1
    val m = Option(v)
    val f: Int => Option[String] = x => Option(x.toString)
    val g: String => Option[Double] = x => Option(x.toDouble)

    "follow left identity law" in leftIdentityLaw(m, v, f)
    "follow right identity law" in rightIdentityLaw(m)
    "follow associativity law" in associativityLaw(m, v, f, g)
  }

  "Try" must {
    import cats.instances.try_._

    val v = 1
    val m = Try(v)
    val f: Int => Try[String] = x => Success(x.toString)
    val g: String => Try[Double] = x => Success(x.toDouble)

    "follow left identity law" in leftIdentityLaw(m, v, f)
    "follow right identity law" in rightIdentityLaw(m)
    "follow associativity law" in associativityLaw(m, v, f, g)

    val badF: Int => Try[String] = _ => Success(throw new Exception)

    "brake left identity law" in {
      println(m.flatMap(badF))
      an [Exception] should be thrownBy m.flatMap(badF) != badF(v)
    }
  }

  import scala.util.Random
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global

  "Future" must {

    "not be referential transparent #1" in {

      val f1 = {
        val r = new Random(0L)
        val x = Future(r.nextInt)
        for {
          a <- x
          b <- x
        } yield (a, b)
      }

      // Same as f1, but I inlined `x`
      val f2 = {
        val r = new Random(0L)
        for {
          a <- Future(r.nextInt)
          b <- Future(r.nextInt)
        } yield (a, b)
      }

      f1.onComplete(println) // Success((-1155484576,-1155484576))
      f2.onComplete(println) // Success((-1155484576,-723955400))    <-- not the same
    }

    "not be referential transparent #2" in {
      for {
        x <- Future { println("Foo") }
        y <- Future { println("Foo") }
      } yield ()

      val printFuture = Future { println("Foo") }

      for {
        x <- printFuture
        y <- printFuture
      } yield ()

    }
  }

  "Task" must {
    import monix.execution.Scheduler.Implicits.global
    "be referential transparent #1" in {
      val task1 = {
        val r = new Random(0L)
        val x = Task.delay(r.nextInt)
        for {
          a <- x
          b <- x
        } yield (a, b)
      }

      // Same as task1, but I inlined `x`
      val task2 = {
        val r = new Random(0L)
        for {
          a <- Task.delay(r.nextInt)
          b <- Task.delay(r.nextInt)
        } yield (a, b)
      }

      task1.foreach { result => println(s"Task1: $result") }// (-1155484576,-723955400)
      task2.foreach { result => println(s"Task2: $result") } // (-1155484576,-723955400)
    }
  }

}

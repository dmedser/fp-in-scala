package lesson16.examples

import cats.implicits._
import cats.data.{ReaderT, Writer}

object example1 extends App {
  val computation = for {
    xs <- ReaderT.ask[Writer[List[Int], ?], List[Int]]
    _ <- ReaderT.liftK(Writer.tell(List(xs(1))))
  } yield xs(2)

  println(computation.run(List(15, 42, 3)).run)
}

package lesson16.examples

object baseExample {
  val x: Option[Int] = Option(5)
  val y: Option[Int] = None
  val z: Option[Int] = for {
    someX <- x
    someY <- y
  } yield someX + someY
}
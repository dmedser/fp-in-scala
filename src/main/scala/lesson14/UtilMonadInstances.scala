package lesson14

object UtilMonadInstances {

  implicit val listMonad: Monad[List] = new Monad[List] {
    def pure[A](a: A): List[A] = List(a)

    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  implicit def eitherMonad[A]: Monad[Either[A, ?]] = new Monad[Either[A, ?]] {
    def pure[B](b: B): Either[A, B] = Right(b)

    def flatMap[B, C](aOrB: Either[A, B])(f: B => Either[A, C]): Either[A, C] =
      aOrB match {
        case Left(a) => Left(a)
        case Right(b) => f(b)
      }
  }
}
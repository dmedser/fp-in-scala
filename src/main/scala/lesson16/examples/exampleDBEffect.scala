package lesson16.examples
import lesson16._

object exampleDBEffect {
  case class DBException(message: String) extends Exception(message)
  case class EntityId(value: Int)
  case class Entity(id: EntityId)

  object example1 {
    type DBEffect[T] = Writer[String, Either[DBException, T]]
    def get(id: EntityId): DBEffect[Entity] = ???

    //  object app1 extends App {
    //    val x: DBEffect[Entity] = ???
    //    val y: DBEffect[Entity] = ???
    //    val pair: DBEffect[(Entity, Entity)]  = for {
    //      someX <- x
    //      someY <- y
    //    } yield (someX, someY)
    //  }
  }

  object example2 {
    case class DBEffect[T](value: Writer[String, Either[DBException, T]])

    object DBEffect {
      implicit def monad[T]: Monad[DBEffect] = new Monad[DBEffect] {
        override def pure[A](x: A): DBEffect[A] = DBEffect(Writer(Right(x) -> ""))

        override def flatMap[A, B](fa: DBEffect[A])(f: A => DBEffect[B]): DBEffect[B] =
          DBEffect(Writer(fa.value.run match {
            case (Right(a), w) =>
              f(a).value.run match {
                case (res, w$) => (res, w ++ w$)
              }
            case (Left(e), w) => (Left(e), w)
          }))

      }
    }
  }

  object example3 {
    import cats.effect.IO

    case class DBEffect[T](run: IO[Either[DBException, T]])

    object DBEffect {
      implicit def monad[T]: Monad[DBEffect] = new Monad[DBEffect] {
        override def pure[A](x: A): DBEffect[A] = DBEffect(IO.pure(Right(x)))

        override def flatMap[A, B](fa: DBEffect[A])(f: A => DBEffect[B]): DBEffect[B] =
          DBEffect {
            fa.run.flatMap {
              case Left(e) => IO.pure(Left(e))
              case Right(a) => f(a).run
            }
          }
      }
    }
  }
}

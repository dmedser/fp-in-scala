package lesson16

import cats.{Semigroup, ~>}

case class IorT[F[_], E, A](run: F[Either[E, Either[(E, A), A]]])

object IorT {
  import cats.syntax.semigroup._

  implicit def iorTMonad[F[_] : Monad, E : Semigroup]: Monad[IorT[F, E, ?]] = new Monad[IorT[F, E, ?]] {
    override def pure[A](a: A): IorT[F, E, A] = IorT(Monad[F].pure(Right(Right(a))))

    override def flatMap[A, B](fa: IorT[F, E, A])(f: A => IorT[F, E, B]): IorT[F, E, B] =
      IorT(
        Monad[F].flatMap(fa.run) {
          case Right(Right(a)) => f(a).run
          case Right(Left((e1, a))) =>
            Monad[F].flatMap(f(a).run) {
              case Right(Right(b)) => Monad[F].pure(Right(Left((e1, b))))
              case Right(Left((e2, b))) => Monad[F].pure(Right(Left(e1 |+| e2, b)))
              case Left(e2) => Monad[F].pure(Left(e1 |+| e2))
            }
          case Left(e) => Monad[F].pure(Left(e))
        }
      )
  }

  implicit def liftK[F[_] : Monad, E]: F ~> IorT[F, E, ?] = new ~>[F, IorT[F, E, ?]] {
    override def apply[A](fa: F[A]): IorT[F, E, A] = IorT(Monad[F].flatMap(fa)(a => Monad[F].pure(Right(Right(a)))))
  }
}
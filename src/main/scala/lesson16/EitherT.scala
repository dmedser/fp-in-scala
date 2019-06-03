package lesson16

import cats.~>
import cats.syntax.functor._
import Monad._

case class EitherT[F[_], E, A](run: F[Either[E, A]])

object EitherT {
  implicit def eitherTMonad[E, F[_] : Monad]: Monad[EitherT[F, E, ?]] = new Monad[EitherT[F, E, ?]] {
    override def pure[A](a: A): EitherT[F, E, A] =
      EitherT { Monad[F].pure[Either[E, A]](Right(a)) }
    //                      Right(a)

    override def flatMap[A, B](fa: EitherT[F, E, A])(f: A => EitherT[F, E, B]): EitherT[F, E, B] = EitherT {
      // fa match {
      //   case Right(a) => f(a)
      //   case Left(e)  => Left(e)
      // }
      fa.run.flatMap {
        case Right(a) => f(a).run
        case Left(e) => Monad[F].pure[Either[E, B]](Left(e))
      }
    }
  }

  def liftK[F[_] : Monad, E]: F ~> EitherT[F, E, ?] = new ~>[F, EitherT[F, E, ?]] {
    override def apply[A](fa: F[A]): EitherT[F, E, A] = EitherT { fa.map[Either[E, A]](Right(_)) }
  }

  def raiseError[E, F[_] : Monad](e: E): EitherT[F, E, Unit] = EitherT(Monad[F].pure[Either[E, Unit]](Left(e)))

  def handleErrorWith[E, F[_] : Monad, A](fe: EitherT[F, E, A])(f: E => EitherT[F, E, A]): EitherT[F, E, A] =
    EitherT {
      fe.run.flatMap {
        case Left(e) => f(e).run
        case Right(v) => Monad[F].pure[Either[E, A]](Right(v))
      }
    }
}
package lesson16

import cats.~>

case class OptionT[F[_], A](run: F[Option[A]])

object OptionT {
  implicit def optionTMonad[F[_] : Monad]: Monad[OptionT[F, ?]] = new Monad[OptionT[F, ?]] {
    override def pure[A](a: A): OptionT[F, A] = OptionT(Monad[F].pure(Some(a)))

    override def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
      OptionT(
        Monad[F].flatMap(fa.run) {
          case Some(a) => f(a).run
          case None => Monad[F].pure(None)
        }
      )
  }

  implicit def liftK[F[_] : Monad]: F ~> OptionT[F, ?] = new ~>[F, OptionT[F, ?]] {
    override def apply[A](fa: F[A]): OptionT[F, A] = OptionT(Monad[F].flatMap(fa)(a => Monad[F].pure(Some(a))))
  }
}

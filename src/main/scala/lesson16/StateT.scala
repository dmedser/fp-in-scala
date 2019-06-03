package lesson16

import cats.~>

case class StateT[F[_], S, A](run: S => F[(A, S)])

object StateT {
  implicit def stateTMonad[F[_] : Monad, S]: Monad[StateT[F, S, ?]] = new Monad[StateT[F, S, ?]] {
    override def pure[A](a: A): StateT[F, S, A] =
      // State { s => (a, s) }
      StateT(s => Monad[F].pure((a, s)))

    override def flatMap[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] =
      // State { s =>
      //   fa.run(s) |> { case (a, s$) => f(a).run(s$) }
      // }
      StateT { s =>
        Monad[F].flatMap(fa.run(s)) { case (a, s_) => f(a).run(s_) }
      }
  }

  def liftK[F[_] : Monad, S]: F ~> StateT[F, S, ?] = new ~>[F, StateT[F, S, ?]] {
    override def apply[A](fa: F[A]): StateT[F, S, A] = StateT(s => Monad[F].flatMap(fa)(a => Monad[F].pure(a, s)))
  }

  def get[F[_] : Monad, S]: StateT[F, S, S] = StateT(s => Monad[F].pure((s, s)))

  def set[F[_] : Monad, S](s: S): StateT[F, S, Unit] = StateT(_ => Monad[F].pure((s, ())))
}

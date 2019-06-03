package lesson16

import cats.Applicative

trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
}

object Monad {
  def apply[F[_]](implicit monad: Monad[F]): Monad[F] = monad

  implicit class MonadOps[F[_] : Monad, A](fa: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = Monad[F].flatMap(fa)(f)
  }

  implicit class MonadFlattenOp[F[_] : Monad, A](ffa: F[F[A]]) {
    def flatten: F[A] = Monad[F].flatten(ffa)
  }

  implicit def applicativeMonad[F[_] : Monad]: Applicative[F] = new Applicative[F] {
    override def pure[A](x: A): F[A] = Monad[F].pure(x)

    override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = ff.flatMap(f => Monad[F].map(fa)(f))
  }

  implicit def idMonad: Monad[Id] = new Monad[Id] {
    override def pure[A](a: A): Id[A] = a

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }
}
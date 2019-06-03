package lesson16

import cats.{Applicative, ~>}
import Monad._

// Reader monad has form of Reader(run: E => A)
case class ReaderT[F[_], E, A](run: E => F[A])
//                                     ^^^^
//                      читаем из окружения
//                                          ^^^^
//                                          на его основе
//                                          конструируем вычисление с эффектом

object ReaderT {
  implicit def readerTMonad[E, F[_] : Monad]: Monad[ReaderT[F, E, ?]] = new Monad[ReaderT[F, E, ?]] {
    override def pure[A](x: A): ReaderT[F, E, A] =
    // Reader { _ => a } = Reader { _ => identity(a) } -- просто заменим identity на pure
      ReaderT { _ => Monad[F].pure(x) }

    override def flatMap[A, B](fa: ReaderT[F, E, A])(f: A => ReaderT[F, E, B]): ReaderT[F, E, B] =
    // Reader { e => fa.run(e) |> (a => f(a).run(e)) } -- просто заменим |> на flatMap
      ReaderT { e => fa.run(e).flatMap(a => f(a).run(e)) }
  }

  def liftK[F[_], E]: F ~> ReaderT[F, E, ?] = new ~>[F, ReaderT[F, E, ?]] {
    override def apply[A](fa: F[A]): ReaderT[F, E, A] = ReaderT { _ => fa }
  }

  def ask[F[_] : Monad, E]: ReaderT[F, E, E] = ReaderT { e => Monad[F].pure(e) }

  def local[F[_] : Monad, E, A](f: E => E)(r: ReaderT[F, E, A]): ReaderT[F, E, A] =
    ReaderT { e => r.run(f(e)) }
}
package lesson16

import cats.{Monoid, ~>}
import cats.syntax.monoid._
import cats.syntax.functor._
import Monad._

// Writer monad has form of Writer((A, W))
case class WriterT[F[_], W, A](run: F[(A, W)])

object WriterT {
  implicit def writerTMonad[F[_] : Monad, W : Monoid]: Monad[WriterT[F, W, ?]] = new Monad[WriterT[F, W, ?]] {
    override def pure[A](a: A): WriterT[F, W, A] =
      WriterT[F, W, A](Monad[F].pure((a, Monoid[W].empty)))

    override def flatMap[A, B](fa: WriterT[F, W, A])(f: A => WriterT[F, W, B]): WriterT[F, W, B] =
      WriterT[F, W, B](fa.run.flatMap { case (a, w) =>
        f(a).run.map { case (b, w$) => (b, w combine w$) }
      })

  }

  def liftK[F[_] : Monad, W : Monoid]: F ~> WriterT[F, W, ?] = new ~>[F, WriterT[F, W, ?]] {
    override def apply[A](fa: F[A]): WriterT[F, W, A] = WriterT { fa.map(a => a -> Monoid[W].empty) }
  }

  def tell[W : Monoid, F[_] : Monad](w: W) = WriterT(Monad[F].pure(((), w)))
}
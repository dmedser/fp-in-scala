package lesson16

import cats.{Functor, Traverse, Distributive}
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.distributive._
import Monad._

case class Nested[F[_], G[_], A](value: F[G[A]])

object Nested {
  implicit def nestedFunctor[F[_], G[_]](implicit F: Functor[F], G: Functor[G]): Functor[λ[a => Nested[F, G, a]]] =
    new Functor[λ[a => Nested[F, G, a]]] {
      def map[A, B](fa: Nested[F, G, A])(f: A => B): Nested[F, G, B] = Nested(fa.value.map((ga: G[A]) => ga.map(f)))
    }

  // implicit def nestedMonad[F[_], G[_]](implicit F: Monad[F], G: Monad[G]): Monad[λ[a => Nested[F, G, a]]] = ???

  implicit def nestedMonadWithTraverse[F[_] : Monad, G[_] : Monad : Traverse]: Monad[Nested[F, G, ?]] =
    new Monad[Nested[F, G, ?]] {
      override def pure[A](a: A): Nested[F, G, A] =
        Nested { Monad[F].pure(Monad[G].pure(a)) }

      // For example `λ[a => Writer[Option[a]]]`
      override def flatMap[A, B](fa: Nested[F, G, A])(f: A => Nested[F, G, B]): Nested[F, G, B] = Nested {
        fa.value // Nested[Writer, Option, A] => Writer[Option[A]]
          .map { ga => // Option[A]
          // traverse: Option[A] => (A => Writer[Option[A]]) => Writer[Option[Option[A]]
          ga.traverse(f(_).value)
        } // Writer[W, Writer[W, Option[Option[A]]]]
          .flatten
          .map(_.flatten)
      }
    }

  implicit def nestedMonadWithDistributive[F[_] : Monad : Distributive, G[_] : Monad]: Monad[Nested[F, G, ?]] =
    new Monad[Nested[F, G, ?]] {
      override def pure[A](a: A): Nested[F, G, A] =
        Nested { Monad[F].pure(Monad[G].pure(a)) }

      // For example `λ[a => Function1[E, Option[a]]]` -- Reader
      override def flatMap[A, B](fa: Nested[F, G, A])(f: A => Nested[F, G, B]): Nested[F, G, B] = Nested {
        Monad[F]
          .map(fa.value) { ga =>
            // distribute: Option[A] => (A => Function1[E, Option[A]]) => Function1[E, Option[Option[A]]
            ga.distribute(f(_).value)
          } // Function[E, Function[E, Option[Option[A]]]]
          .flatten |> (fggb => Monad[F].map(fggb)(_.flatten))
      }
    }
}
package lesson9

import cats.{~>, Functor}
import cats.arrow.FunctionK
import cats.syntax.functor._

/*
Yoneda[F, ?] is a cofree functor for F. It picks up functoriality from F.

let F[_] : Functor
lift:
  forall A B. (A => B) => (F[A] => F[B]) ==
  forall A B. (A => B, F[A]) => F[B] ==
  forall A B. (F[A], A => B) => F[B] ==
  forall A B. F[A] => (A => B) => F[B] ==
  forall A. F[A] => (forall B. (A => B) => F[B]) ==
  forall A. F[A] => ((A => ?) ~> F)

liftYoneda:
forall A. F[A] => Yoneda(run: (A => ?) ~> F)
*/
case class Yoneda[F[_], A](run: (A => ?) ~> F) {
  def lower: F[A] = run(identity[A])
}

object Yoneda {
  def lift[F[_] : Functor, A](fa: F[A]): Yoneda[F, A] = Yoneda(λ[(A => ?) ~> F](f => fa.map(f)))

  implicit def functorForYoneda[F[_]]: Functor[Yoneda[F, ?]] = new Functor[Yoneda[F, ?]] {
    def map[A, B](ya: Yoneda[F, A])(f: A => B): Yoneda[F, B] = Yoneda(λ[(B => ?) ~> F](g => ya.run(g compose f)))
  }
}

/*
Coyoneda[F, ?] is a free functor for F. It gives functoriality to any F.

let F[_] : Functor
lift:
  forall A B. (A => B) => (F[A] => F[B]) ==
  forall A B. (A => B, F[A]) => F[B] ==
  forall A B. (F[A], A => B) => F[B] ==
  forall B. (forall A. (F[A], (A => B)) => F[B] ==
  forall A. (forall R. (F[R], (R => A)) => F[A] ==

lowerCoyoneda:
forall A. Coyoneda(unwrap: (F[R], R => A) forSome { type R }) => F[A]
*/
case class Coyoneda[F[_], A](unwrap: (F[R], R => A) forSome { type R }) {
  def lower(implicit F: Functor[F]): F[A] = foldMap(FunctionK.id)

  def foldMap[G[_] : Functor](k: F ~> G): G[A] = unwrap match {
    case (fr, f) => k(fr).map(f)
  }
}

object Coyoneda {
  def lift[F[_], A](fa: F[A]): Coyoneda[F, A] = Coyoneda((fa, identity[A] _))

  implicit def functorForCoyoneda[F[_]]: Functor[Coyoneda[F, ?]] = new Functor[Coyoneda[F, ?]] {
    def map[A, B](ca: Coyoneda[F, A])(f: A => B): Coyoneda[F, B] = ca.unwrap match {
      case (fr, g) => Coyoneda((fr, f compose g))
    }
  }
}
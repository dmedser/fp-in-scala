package lesson11

import cats.syntax.either._
import cats.{Apply, Monoid}
import lesson10.Applikative
import lesson8.Funktor

/*
@runarorama
There are two basic answers to "how do I" questions in Scala. One is "don't do that". The other is "traverse".

https://twitter.com/runarorama/status/864860002569392128
 */

// One of the most common problems: given a list of Futures, how to get a Future with the list of results?
// List[Future[A]] => Future[List[A]]

// We use Future.sequence(List(fa, fb, ...)). Can we abstract over Future and List?

trait Traversable[F[_]] /* extends Functor[F] with Foldable[F] */ {

  def traverse[G[_] : Applikative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def sequence[G[_] : Applikative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity[G[A]])

  // Functor and Foldable can be derived via Traversable
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[λ[α => α], A, B](fa)(f)

  def foldMap[A, B : Monoid](fa: F[A])(f: A => B): B = traverse[λ[α => B], A, B](fa)(f)

  // traverse can be expressed via map & sequence
  //def traverse[G[_] : Applikative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

  // laws
  // 1) identity: fa.traverse(G.pure) == fa.pure[G]
  // 3) compose:  fa.traverse(f: A => G[B]).map(_.traverse(g: B => H[C])) == fa.traverse(g ∘ f)
}

object Traversable {
  def apply[F[_] : Traversable]: Traversable[F] = implicitly

  // concrete traversable examples
  implicit val optionTraversable: Traversable[Option] = new Traversable[Option] {
    def traverse[G[_] : Applikative, A, B](option: Option[A])(f: A => G[B]): G[Option[B]] =
      option.fold(Applikative[G].pure(Option.empty[B]))(a ⇒ Applikative[G].map(f(a))(Some(_)))
  }

  implicit val listTraversable: Traversable[List] = new Traversable[List] {
    def traverse[G[_] : Applikative, A, B](list: List[A])(f: A => G[B]): G[List[B]] = list match {
      case Nil => Applikative[G].pure(List.empty[B])
      case head :: tail => Applikative[G].map2(f(head), traverse(tail)(f))(_ :: _)
    }
  }

  // universal traversable derivation rules
  implicit val idTraversable: Traversable[λ[α => α]] = new Traversable[λ[α => α]] {
    def traverse[G[_] : Applikative, A, B](a: A)(f: A => G[B]): G[B] = f(a)
  }

  implicit def constTraversable[C]: Traversable[λ[α => C]] = new Traversable[λ[α => C]] {
    def traverse[G[_] : Applikative, A, B](c: C)(f: A => G[B]): G[C] = Applikative[G].pure(c)
  }

  implicit def prodTraversable[F[_] : Traversable, G[_] : Traversable]: Traversable[λ[α => (F[α], G[α])]] =
    new Traversable[λ[α => (F[α], G[α])]] {
      def traverse[H[_] : Applikative, A, B](faga: (F[A], G[A]))(f: A => H[B]): H[(F[B], G[B])] = {
        val fa = faga._1
        val ga = faga._2
        val gfb = Traversable[F].traverse(fa)(f)
        val ggb = Traversable[G].traverse(ga)(f)
        Applikative[H].map2(gfb, ggb)((fb, gb) => (fb, gb))
      }
    }

  implicit def sumTraversable[F[_] : Traversable, G[_] : Traversable]: Traversable[λ[α => F[α] Either G[α]]] =
    new Traversable[λ[α => F[α] Either G[α]]] {
      def traverse[H[_] : Applikative, A, B](faga: F[A] Either G[A])(f: A => H[B]): H[F[B] Either G[B]] =
        faga.fold(
          fa => Applikative[H].map(Traversable[F].traverse(fa)(f))(_.asLeft[G[B]]),
          ga => Applikative[H].map(Traversable[G].traverse(ga)(f))(_.asRight[F[B]])
        )
    }

  implicit def compTraversable[F[_] : Traversable, G[_] : Traversable]: Traversable[λ[α => F[G[α]]]] =
    new Traversable[λ[α => F[G[α]]]] {
      def traverse[H[_] : Applikative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
        Traversable[F].traverse(fga)(ga => Traversable[G].traverse(ga)(f))
    }

  // Hence, all finite polynomials are traversable

  // What about exponentials?
  implicit def simplestExpTraversable[R]: Traversable[R => ?] = new Traversable[R => ?] {
    def traverse[G[_] : Applikative, A, B](ra: R => A)(f: A => G[B]): G[R => B] = ???
  }

  // And infinite structures?
  implicit val streamTraversable: Traversable[Stream] = new Traversable[Stream] {
    def traverse[G[_] : Applikative, A, B](fa: Stream[A])(f: A => G[B]): G[Stream[B]] = ???
  }

}

// Why did we require Applicative? Only for constTraversable.
// If a polynomial F doesn't contain "empty" part, we can reduce requirement to Apply.

trait Traversable1[F[_]] { // NonEmptyTraverse in cats
  def traverse1[G[_] : Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}

// Traversable functor F (sequence) allows us to push its structure into any applicative functor G:
// given (Traversable[F], Applicative[G]) => F ∘ G ~> G ∘ F

// Can we do the opposite - distribute F over G?
// given (Distributive[F], Functor[G]) => G ∘ F ~> F ∘ G

// It's possible for Distributive functors

trait Distributive[F[_]] extends Funktor[F] {

  def distribute[G[_] : Funktor, A, B](ga: G[A])(f: A => F[B]): F[G[B]]

  // dual of sequence
  def cosequence[G[_] : Funktor, A](gfa: G[F[A]]): F[G[A]] = distribute(gfa)(identity[F[A]])

  // dual of traverse
  def cotraverse[G[_] : Funktor, A, B](gfb: G[F[B]])(f: G[B] => A): F[A] = map(cosequence(gfb))(f)

  // distribute can be expressed via map & cosequence
  //def distribute[G[_] : Funktor, A, B](ga: G[A])(f: A => F[B]): F[G[B]] = cosequence(Funktor[G].map(ga)(f))

}

object Distributive {
  def apply[F[_]: Distributive]: Distributive[F] = implicitly

  // All representable functors are distributive
  // representable functor is such that: F[A] <-> R => A
  implicit def reprDistributive[R]: Distributive[R => ?] = new Distributive[R => ?] {
    def distribute[G[_] : Funktor, A, B](ga: G[A])(f: A => R => B): R => G[B] = r => Funktor[G].map(ga)(a => f(a)(r))

    def lift[A, B](f: A => B): (R => A) => R => B = ra => r => f(ra(r))
  }

  implicit def prodDistributive[F[_] : Distributive, G[_] : Distributive]: Distributive[λ[α => (F[α], G[α])]] =
    new Distributive[λ[α => (F[α], G[α])]] {
      def distribute[H[_] : Funktor, A, B](ha: H[A])(f: A => (F[B], G[B])): (F[H[B]], G[H[B]]) =
        (Distributive[F].distribute(ha)(f andThen { case (fb, _) => fb }), Distributive[G].distribute(ha)(f andThen {
          case (_, gb) => gb
        }))

      def lift[A, B](f: A => B): ((F[A], G[A])) => (F[B], G[B]) = {
        case (fa, ga) => (Distributive[F].map(fa)(f), Distributive[G].map(ga)(f))
      }
    }

  implicit def compDistributive[F[_] : Distributive, G[_] : Distributive]: Distributive[λ[α => F[G[α]]]] =
    new Distributive[λ[α => F[G[α]]]] {

      // TODO
      def distribute[H[_] : Funktor, A, B](ha: H[A])(f: A => F[G[B]]): F[G[H[B]]] = ???

      def lift[A, B](f: A => B): F[G[A]] => F[G[B]] = fga => Distributive[F].map(fga)(Distributive[G].lift(f))
    }

  // what about sum?
  implicit def sumDistributive[F[_] : Distributive, G[_] : Distributive]: Distributive[λ[α => F[α] Either G[α]]] =
    new Distributive[λ[α => Either[F[α], G[α]]]] {
      def distribute[H[_] : Funktor, A, B](ha: H[A])(f: A => Either[F[B], G[B]]): Either[F[H[B]], G[H[B]]] = ???

      def lift[A, B](f: A => B): Either[F[A], G[A]] => Either[F[B], G[B]] = {
        case Left(fa) => Distributive[F].map(fa)(f).asLeft[G[B]]
        case Right(ga) => Distributive[G].map(ga)(f).asRight[F[B]]
      }
    }
}

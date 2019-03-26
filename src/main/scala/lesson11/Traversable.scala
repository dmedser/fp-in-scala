package lesson11

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
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[λ[α ⇒ α]/*Id*/, A, B](fa)(f)
  def foldMap[A, B : Monoid](fa: F[A])(f: A => B): B = ??? //traverse[???, A, B](fa)(f) // G applikative instance

  // traverse can be expressed via map & sequence
  //def traverse[G[_] : Applikative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
  //  sequence(map(fa)(f))

  // laws
  // 1) identity: fa.traverse(G.pure) == fa.pure[G]
  // 3) compose:  fa.traverse(f: A => G[B]).map(_.traverse(g: B => H[C])) == fa.traverse(g ∘ f)
}

object Traversable {
  def apply[F[_]](implicit ev: Traversable[F]): Traversable[F] = ev

  // concrete functor examples
  implicit val optionTraversable: Traversable[Option] = new Traversable[Option] {
    def traverse[G[_] : Applikative, A, B](fa: Option[A])(f: A ⇒ G[B]): G[Option[B]] =
      fa.fold(Applikative[G].pure(Option.empty[B]))(a ⇒ Applikative[G].map(f(a))(Some(_)))

  }

  implicit val listTraversable: Traversable[List] = ???

  // universal traversable derivation rules
  implicit val idTraversable: Traversable[λ[α => α]] = new Traversable[λ[α => α]] {
    def traverse[G[_] : Applikative, A, B](fa: A)(f: A ⇒ G[B]): G[B] = f(fa)
  }

  implicit def constTraversable[C]: Traversable[λ[α => C]] = new Traversable[λ[α => C]] {
    def traverse[G[_] : Applikative, A, B](fa: C)(f: A ⇒ G[B]): G[C] = Applikative[G].pure(fa)
  }

  implicit def prodTraversable[F[_] : Traversable, G[_] : Traversable]: Traversable[λ[α => (F[α], G[α])]] = ???

  implicit def sumTraversable[F[_] : Traversable, G[_] : Traversable]: Traversable[λ[α => F[α] Either G[α]]] = ???

  implicit def compTraversable[F[_] : Traversable, G[_] : Traversable]: Traversable[λ[α => F[G[α]]]] = ???

  // Hence, all finite polynomials are traversable

  // What about exponentials?
  implicit def simplestExpTraversable[R]: Traversable[R => ?] = new Traversable[R => ?] {
    def traverse[G[_] : Applikative, A, B](fa: R ⇒ A)(f: A ⇒ G[B]): G[R ⇒ B] = ??? //f.compose(fa)
  }

  // And infinite structures?
  implicit val streamTraversable: Traversable[Stream] = ???

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
  def cosequence[G[_] : Funktor, A](gfa: G[F[A]]): F[G[A]] = distribute(gfa)(identity)

  // dual of traverse
  def cotraverse[G[_] : Funktor, A, B](gfb: G[F[B]])(f: G[B] => A): F[A] = map(cosequence(gfb))(f)

  // distribute can be expressed via map & cosequence
  // def distribute[G[_] : Funktor, A, B](fa: G[A])(f: A => F[B]): F[G[B]] = ???

}

object Distributive {
  def apply[F[_]](implicit ev: Distributive[F]): Distributive[F] = ev

  // All representable functors are distributive
  // representable functor is such that: F[A] <-> R => A
  implicit def reprDistributive[R]: Distributive[R => ?] = new Distributive[R => ?] {
    def distribute[G[_] : Funktor, A, B](ga: G[A])(f: A ⇒ R ⇒ B): R ⇒ G[B] =
      r ⇒ Funktor[G].map(ga)(a ⇒ f(a)(r))

    def lift[A, B](f: A ⇒ B): (R ⇒ A) ⇒ R ⇒ B = ra ⇒ r ⇒ f(ra(r))
  }

  implicit def prodDistributive[F[_] : Distributive, G[_] : Distributive]: Distributive[λ[α => (F[α], G[α])]] = ???
  implicit def compDistributive[F[_] : Distributive, G[_] : Distributive]: Distributive[λ[α => F[G[α]]]] = ???

  // what about sum?
  implicit def sumDistributive[F[_] : Distributive, G[_] : Distributive]: Distributive[λ[α => F[α] Either G[α]]] = ???

}


// Homework:
// create ADT for a binary tree with values in branches and leaves
// write traversable instances that traverse a tree in DFS (inorder, preorder, postorder) and BFS.
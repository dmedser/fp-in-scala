package lesson14

import cats.Semigroup
import lesson10.Applikative
import lesson11.Traversable
import lesson8.Funktor

object `package` {
  type :+:[+A, +B] = Either[A, B]
  type :*:[+A, +B] = (A, B)
  type :∘:[F[_], G[_]] = { type λ[α] = F[G[α]] }
  type :++:[F[_], G[_]] = { type λ[α] = F[α] :+: G[α] }
  type Id[A] = A
}

// Two alternative definition of Monads
// 1) Functor + pure + flatten
// 2) pure + flatMap

trait Monad[F[_]] {

  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

  // Functor and Applicative can be expressed via Monad
  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))

}

object Monad {

  def apply[F[_] : Monad]: Monad[F] = implicitly

  implicit val idMonad: Monad[λ[α => α]] = new Monad[λ[α => α]] {
    def pure[A](a: A): Id[A] = a

    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }

  implicit def aAndFMonad[F[_] : Monad]: Monad[λ[α => α :*: F[α]]] = new Monad[λ[α => α :*: F[α]]] {
    def pure[A](a: A): A :*: F[A] = (a, Monad[F].pure(a))

    def flatMap[A, B](aAndF: A :*: F[A])(f: A => B :*: F[B]): B :*: F[B] = {
      val a = aAndF._1
      val fa = aAndF._2
      (f(a)._1, Monad[F].flatMap(fa)(f.andThen(_._2)))
    }
  }

  implicit def aOrFMonad[F[_] : Monad]: Monad[λ[α => α :+: F[α]]] = new Monad[λ[α => α :+: F[α]]] {
    def pure[A](a: A): A :+: F[A] = Left(a)

    def flatMap[A, B](aOrF: A :+: F[A])(f: A => B :+: F[B]): B :+: F[B] =
      aOrF match {
        case Left(a) => f(a)
        case Right(fa) =>
          Right(Monad[F].flatMap(fa) { a =>
            f(a) match {
              case Left(b) => Monad[F].pure(b)
              case Right(fb) => fb
            }
          })
      }
  }

  implicit def fExpRMonad[R, F[_] : Monad]: Monad[λ[α => R => F[α]]] = new Monad[λ[α => R => F[α]]] {
    def pure[A](a: A): R => F[A] = (_: R) => Monad[F].pure(a)

    def flatMap[A, B](rToFa: R => F[A])(f: A => R => F[B]): R => F[B] =
      r => Monad[F].flatMap(rToFa(r))(a => f(a)(r))
  }

  implicit def prod2Monads[F[_] : Monad, G[_] : Monad]: Monad[λ[α => F[α] :*: G[α]]] =
    new Monad[λ[α => F[α] :*: G[α]]] {
      def pure[A](a: A): F[A] :*: G[A] = (Monad[F].pure(a), Monad[G].pure(a))

      def flatMap[A, B](faga: F[A] :*: G[A])(f: A => F[B] :*: G[B]): F[B] :*: G[B] =
        (Monad[F].flatMap(faga._1)(f andThen (_._1)), Monad[G].flatMap(faga._2)(f andThen (_._2)))
    }
  // there are many more, but they are no rules(?) to construct a lawful monad

  // monads do not compose in general (but they do if one monad can be restricted - how?)
  implicit def composeMonad[F[_] : Monad : Applikative, G[_] : Monad : Traversable]: Monad[λ[α => F[G[α]]]] =
    new Monad[λ[α => F[G[α]]]] {
      def pure[A](a: A): F[G[A]] = Monad[F].pure(Monad[G].pure(a))

      def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        Monad[F].map(
          Monad[F].flatten(
            Monad[F]
              .map[G[A], F[G[G[B]]]](fga)(ga => Traversable[G].sequence(Monad[G].map[A, F[G[B]]](ga)(a => f(a))))
          )
        )(ggb => Monad[G].flatten(ggb))
    }
  // monads compose with the aid of monad transformers - see the next lessons

}

// Multi-value monads
case class Lst[A](unwrap: Unit :+: (A, Lst[A]))
// Semantics: a computation with several results
// pure: List(a)
// flatten: ((1), (2, 3), (4, 5, 6)) -> (1, 2, 3, 4, 5, 6)

// Tree-like structures
case class BinTree[A](unwrap: A :+: (BinTree[A], BinTree[A]))

// Tree, whose shape is determined by F
case class FTree[F[_], A](unwrap: A :+: F[FTree[F, A]])
object FTree {
  import cats.syntax.either._

  implicit def monad[F[_] : Funktor]: Monad[FTree[F, ?]] = new Monad[FTree[F, ?]] {
    def pure[A](a: A): FTree[F, A] = FTree(Left(a))

    def flatMap[A, B](fa: FTree[F, A])(f: A => FTree[F, B]): FTree[F, B] =
      fa.unwrap match {
        case Left(a) => f(a)
        case Right(fftreea) => FTree[F, B](Funktor[F].map(fftreea)(ftreea => flatMap(ftreea)(f)).asRight[B])
      }
  }
}
// Can you see similarity with lesson10.Free? We'll return to it later

// Single-value monads (the rest)
case class Ior[E, A](unwrap: E :+: (E, A) :+: A)
object Ior {
  implicit def monad[E : Semigroup]: Monad[Ior[E, ?]] = new Monad[Ior[E, ?]] {
    def pure[A](a: A): Ior[E, A] = Ior(Right(Right(a)))

    def flatMap[A, B](fa: Ior[E, A])(f: A => Ior[E, B]): Ior[E, B] = ???
  }
}
// Semantics:
// Ior can be either:
//  1) pure value
//  2) critical error
//  3) value with a soft error
//
//  (E1, A) >> (E2, B) -> (E1+E2, B)
//  (E1, A) >> E2 -> E1+E2
//  E >> _ -> E

// How to make an additive E' of any E? Via the free Semigroup - e.g. NonEmptyChain.
// Ior often goes with Nel/Nec on the left.

case class Eval[A](unwrap: A :+: (() => A))
// Semantics: composition of lazy computations
// Explicitly encodes laziness as a data type rather than: lazy val a, a: => A, etc.
// Used in Cats, e.g. for lazy folds. See: `foldr`

// Separate topic: stack-safety
// We'll learn it later.

case class Cont[R, A](unwrap: (A => R) => R)
// Semantics: composition of CPS computations
// Very hard to reason about! Not very useful in Scala: still has issues with stack-safety, even the latest Cats version.
// You are actually familiar with Cont if ever used callback-based APIs.

// Bind semantics:
// (A => R) => R >>= A => (B => R) => R   ->    (B => R) => R
// (A>   >R)R>   >>= >A(B>  >R)R>         ->    (A>>A(B>  >R)R>>R)R>
// ...
// ->  (A>>A(B>>B(C>  >R)R>>R)R>>R)R>

// Typically we write a function as:
// { cb =>
//    here we can do some computation of type A and pass it to the callback 0 or 1 or more times
// }
object Cont {
  implicit def monad[R]: Monad[Cont[R, ?]] = ???

  // call with current continuation, gives the explicit control on continuation
  def callCC[R, A, B](k: (A => Cont[R, B]) => Cont[R, A]): Cont[R, A] =
    Cont(c => k(x => Cont(_ => c(x))).unwrap(c))
}

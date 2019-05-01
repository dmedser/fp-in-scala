package lesson14

import cats.{~>, Monoid}
import lesson8.Funktor

// Adjunctions:
// Free -| Forgetful

// Free - free functor for algebra Alg
// Forgetful U - forgets algebra Alg

// Free x -> y  <=>  x -> U y
// foldMap :: (x -> U y) -> Free x -> y, where y has Alg

// foldMap :: Alg y => Free x -> (x -> y) -> y
// def foldMap[A, B : Alg](fa: Free[A])(f: A => B): B

// Free monoid: category of types => category of monoids
case class FreeMonoid[A](unwrap: Unit :+: A :*: FreeMonoid[A]) {
  def foldMap[B : Monoid](f: A => B): B = ???
}
// Recursive structure, allows building any sequence of combined As:
// Unit
// A :*: Unit
// A :*: A :*: Unit
// A :*: ... :*: A :*: Unit
// :*: is associative, Unit is neutral. Easy to implement Monoid algebra.


// Free monad - also a monoid, but a bit different: category of functors => category of monads
case class FreeMonad[F[_] : Funktor, A](unwrap: Id[A] :+: F[FreeMonad[F, A]]) { // Id :++: F :∘: FreeMonad[F, ?]
  def foldMap[G[_] : Monad](f: F ~> G): G[A] = ???
}
// Recursive structure, allows building any sequence of composed Fs:
// Id
// F :∘: Id
// F :∘: F :∘: Id
// F :∘: ... :∘: F :∘: Id
// :∘: is associative, Id is neutral. Given F is a functor, it's easy to implement Monad algebra (pure + flatten).

// Functor constraint is easy to overcome. Almost everything is a functor, or we can lift any type constructor to Coyoneda.

// Some examples of FreeMonads.
object Frees {
  type Lst[A] = FreeMonad[(A, ?), Unit]
  type Evl[A] = FreeMonad[() => ?, A]
  type BinTree[A] = FreeMonad[λ[α => (α, α)], A]
  type Opt[A] = FreeMonad[λ[α => A], Unit]
  type Opt2[A] = FreeMonad[λ[α => Unit], A]
  type Eith[E, A] = FreeMonad[λ[α => A], E]
}

// Looks pretty cool and it is... in Haskell.
// But in Scala we have stack issues and an overhead on extra allocations.
// Just as folding a list might be not stack-safe, folding a huge program encoded as a free monad might cause SOE.
//
// http://days2012.scala-lang.org/sites/days2012/files/bjarnason_trampolines.pdf
// http://functorial.com/stack-safety-for-free/index.pdf
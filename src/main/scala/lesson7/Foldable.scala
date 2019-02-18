package lesson7

import cats.Monoid
import cats.syntax.semigroup._
import cats.implicits._

trait Foldable[F[_]] {

  type Endo[A] = A ⇒ A

  def endoMonoid[A](f: (Endo[A], Endo[A]) ⇒ Endo[A]): Monoid[Endo[A]] = new Monoid[Endo[A]] {
    def empty: Endo[A] = identity
    def combine(x: Endo[A], y: Endo[A]): Endo[A] = f(x, y)
  }

  implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = true
    def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty: Boolean = false
    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }


  // Часть 1.
  def foldMap[A, C: Monoid](fa: F[A])(f: A => C): C = {
    foldr(fa)(implicitly[Monoid[C]].empty) { case (a, c) ⇒ f(a) |+| c }
  }

  def foldMap[A, C: Monoid](fa: F[A])(f: A => C): C = {
    foldl(fa)(implicitly[Monoid[C]].empty) { case (c, a) ⇒ c |+| f(a) }
  }

  def foldr[A, B](fa: F[A])(z: B)(f: (A, B) => B): B = {
    foldMap(fa) { a: A ⇒ (b: B) ⇒ f(a, b) }(endoMonoid(_ compose _)).apply(z)
  }

  def foldl[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = {
    foldMap(fa) { a: A ⇒ (b: B) ⇒ f(b, a) }(endoMonoid(_ andThen _)).apply(z)
  }

  def foldr[A, B](fa: F[A])(z: B)(f: (A, B) => B): B = {
    foldl(fa)(z)((b: B, a: A) ⇒ f(a, b))
  }

  def foldl[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = {
    foldr(fa)(z)((a: A, b: B) ⇒ f(b, a))
  }


  // Часть 2.
  def headOption[A](fa: F[A]): Option[A] = {
    foldr(fa)(Option.empty[A])((a, b) ⇒ Some(a))
  }

  def lastOption[A](fa: F[A]): Option[A] = {
    foldl(fa)(Option.empty[A])((a, b) ⇒ Some(b))
  }

  def length[A](fa: F[A]): Int = {
    foldl(fa)(0)((a, b) ⇒ a + 1)
  }

  def exists[A](fa: F[A])(p: A => Boolean): Boolean = {
    foldMap(fa)(p)(booleanOrMonoid)
  }

  def forall[A](fa: F[A])(p: A => Boolean): Boolean = {
    foldMap(fa)(p)(booleanAndMonoid)
  }


  // Часть 3.
  def foldrN[A, B](fa: F[A])(n: Int)(z: B)(f: (A, B) => B): B = {

    foldr(fa)((length(fa), ))

  }

}

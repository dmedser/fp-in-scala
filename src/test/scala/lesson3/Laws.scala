package lesson3

import cats.{FlatMap, Monad}
import org.scalatest.{Assertion, MustMatchers}
import scala.language.higherKinds

object Laws extends MustMatchers {

  import cats.syntax.flatMap._
  def leftIdentityLaw[A, B, M[_] : FlatMap](m: M[A], v: A, f: A => M[B]): Assertion = m.flatMap(f) mustBe f(v)

  def rightIdentityLaw[A, M[_]](m: M[A])(implicit M: Monad[M]): Assertion = m.flatMap(M.pure) mustBe m

  def associativityLaw[A, B, C, M[_] : FlatMap](m: M[A], v: A, f: A => M[B], g: B => M[C]): Assertion =
    m.flatMap(f).flatMap(g) mustBe m.flatMap(x => f(x).flatMap(g))

}

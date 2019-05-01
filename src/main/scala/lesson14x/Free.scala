package lesson14x

import cats.{~>, Monad, StackSafeMonad}
import cats.arrow.FunctionK

import scala.annotation.tailrec
import scala.util.{Left, Right}

sealed trait Free[F[_], A] {

  def map[B](f: A => B): Free[F, B] = FlatMap(this, (a: A) => Pure(f(a)))

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

  @tailrec // https://www.janis-voigtlaender.eu/papers/AsymptoticImprovementOfComputationsOverFreeMonads.pdf
  final def codensify: Free[F, A] = this match {
    case FlatMap(FlatMap(fx, f), g) => fx.flatMap(x => f(x).flatMap(g)).codensify
    case FlatMap(Pure(a), f) => f(a).codensify
    case x => x
  }

  def foldMap[G[_]](k: F ~> G)(implicit G: Monad[G]): G[A] =
    G.tailRecM(this)(_.codensify match {
      case Pure(a) => G.pure(Right(a))
      case LiftF(fa) => G.map(k(fa))(Right(_))
      case FlatMap(fa, f) => G.map(fa.foldMap(k))(a => Left(f(a)))
    })

  def fold(implicit F: Monad[F]): F[A] = foldMap(FunctionK.id)

}

object Free {
  def pure[F[_], A](a: A): Free[F, A] = Pure(a)

  def susp[F[_], A](e: => Free[F, A]): Free[F, A] = Free.pure[F, Unit](()).flatMap(_ => e)

  implicit def freeMonad[F[_]]: Monad[Free[F, ?]] = new Monad[Free[F, ?]] with StackSafeMonad[Free[F, ?]] {
    def pure[A](a: A): Free[F, A] = Free.pure(a)
    override def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] = fa.map(f)
    def flatMap[A, B](a: Free[F, A])(f: A => Free[F, B]): Free[F, B] = a.flatMap(f)
  }
}

case class Pure[F[_], A](a: A) extends Free[F, A]
case class LiftF[F[_], A](fa: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](fa: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object FreeTest extends App {

  import cats.implicits._

  type Evl[A] = Free[() => ?, A]
  def susp[A](a: => Evl[A]): Evl[A] = Free.susp[() => ?, A](a)

  def foldR[A, B](list: List[A])(z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case h :: t => f(h, foldR(t)(z)(f))
  }
  //println(foldR((1 to 20000).toList)(0)(_ + _))  // Runtime Error


  def foldREvl[A, B](list: List[A])(z: Evl[B])(f: (A, Evl[B]) => Evl[B]): Evl[B] = list match {
    case Nil => z
    case head :: tail => f(head, susp(foldREvl(tail)(z)(f)))
  }
  println(foldREvl((1 to 20000).toList)(Free.pure[() => ?, Int](0))((i, e) => e.map(_ + i)).fold.apply())

}
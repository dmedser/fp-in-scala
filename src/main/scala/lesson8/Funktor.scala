package lesson8

import cats.Id

trait Funktor[F[_]] { self ⇒

  def lift[A, B](f: A ⇒ B): F[A] ⇒ F[B] = fa ⇒ map(fa)(f)

  def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = lift(f)(fa)

  def void[A](fa: F[A]): F[Unit] = map(fa)(_ ⇒ ())

  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ ⇒ b)

  def widen[A, B >: A](fa: F[A]): F[B] = map(fa)(value ⇒ value)

  def tupleLeft[A, B](fa: F[A], b: B): F[(B, A)] = map(fa)(value ⇒ (b, value))

  def tupleRight[A, B](fa: F[A], b: B): F[(A, B)] = map(fa)(value ⇒ (value, b))

  def fproduct[A, B](fa: F[A])(f: A ⇒ B): F[(A, B)] = map(fa)(value ⇒ (value, f(value)))

  def compose[G[_]](implicit G: Funktor[G]): Funktor[Lambda[x ⇒ F[G[x]]]] = new Funktor[Lambda[x ⇒ F[G[x]]]]{
    override def lift[A, B](f: A ⇒ B): F[G[A]] ⇒ F[G[B]] = self.lift(G.lift(f))
  }

}

object Funktor {
  implicit val optionFunktor: Funktor[Option] = new Funktor[Option] {
    override def lift[A, B](f: A ⇒ B): Option[A] ⇒ Option[B] = {
      case None ⇒ None
      case Some(a) ⇒ Some(f(a))
    }
  }

  implicit val idFunktor: Funktor[Id] = new Funktor[Id] {
    override def lift[A, B](f: A ⇒ B): Id[A] ⇒ Id[B] = f
  }

  type Const[C, A] = C

  implicit def constFunktor[C]: Funktor[Const[C, ?]] = new Funktor[Const[C, ?]] {
    override def lift[A, B](f: A ⇒ B): C ⇒ C /*Const[C, ?] ⇒ Const[C, ?]*/ = c ⇒ c //identity
  }

  implicit def tupleFunktor[F[_], G[_]](implicit F: Funktor[F], G: Funktor[G]): Funktor[Lambda[x ⇒ (F[x], G[x])]] = ???

  implicit def eitherFunktor[F[_], G[_]](implicit F: Funktor[F], G: Funktor[G]): Funktor[Lambda[x ⇒ Either[F[x], G[x]]]] = ???

  implicit def functionFunktor[R, F[_]](implicit F: Funktor[F]): Funktor[Lambda[x ⇒ R ⇒ F[x]]] = ???

  implicit def contraFunktor[F[_], G[_]](implicit F: Contra[F], G: Funktor[G]): Funktor[Lambda[x ⇒ F[x] ⇒ G[x]]] = ???

}

trait Contra[F[_]] {
  def contraLift[A, B](f: A ⇒ B): F[B] ⇒ F[A]
}
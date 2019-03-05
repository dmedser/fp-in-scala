package lesson8

import cats.Id

trait Funktor[F[_]] { /*self ⇒*/

  def lift[A, B](f: A ⇒ B): F[A] ⇒ F[B] /*= fa ⇒ map(fa)(f)*/

  def map[A, B](fa: F[A])(f: A ⇒ B): F[B] = lift(f)(fa)

  def void[A](fa: F[A]): F[Unit] = map(fa)(_ ⇒ ())

  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ ⇒ b)

  def widen[A, B >: A](fa: F[A]): F[B] = map(fa)(value ⇒ value)

  def tupleLeft[A, B](fa: F[A], b: B): F[(B, A)] = map(fa)(value ⇒ (b, value))

  def tupleRight[A, B](fa: F[A], b: B): F[(A, B)] = map(fa)(value ⇒ (value, b))

  def fproduct[A, B](fa: F[A])(f: A ⇒ B): F[(A, B)] = map(fa)(value ⇒ (value, f(value)))

  /*
  def compose[G[_]](implicit G: Funktor[G]): Funktor[λ[α ⇒ F[G[α]]]] = new Funktor[λ[α ⇒ F[G[α]]] ]{
    def lift[A, B](f: A ⇒ B): F[G[A]] ⇒ F[G[B]] = self.lift(G.lift(f))
   }
  */

}

trait ContraFunktor[F[_]] {
  def contraLift[A, B](f: B ⇒ A): F[A] ⇒ F[B]

  def contraMap[A, B](fa: F[A])(f: B ⇒ A): F[B]
}

object Funktor {
  implicit val optionFunktor: Funktor[Option] = new Funktor[Option] {
    def lift[A, B](f: A ⇒ B): Option[A] ⇒ Option[B] = {
      case None ⇒ None
      case Some(a) ⇒ Some(f(a))
    }
  }

  /*λ[α => α]*/

  implicit val idFunktor: Funktor[Id] = new Funktor[Id] {
    def lift[A, B](f: A ⇒ B): Id[A] ⇒ Id[B] = f
  }

  implicit val listFunktor: Funktor[List] = new Funktor[List] {
    def lift[A, B](f: A ⇒ B): List[A] ⇒ List[B] = listA ⇒ listA.map(f)
  }

  /*
    type Const[C, A] = C

    implicit def constFunktor[C]: Funktor[Const[C, ?]] = new Funktor[Const[C, ?]] {
      def lift[A, B](f: A ⇒ B): C ⇒ C /*Const[C, ?] ⇒ Const[C, ?]*/ = c ⇒ c //identity
    }
  */

  implicit def constFunktor[C]: Funktor[λ[α ⇒ C]] = new Funktor[λ[α ⇒ C]] {
    def lift[A, B](f: A ⇒ B): C ⇒ C = ???
  }

  implicit def prodFunktor[F[_], G[_]](implicit F: Funktor[F], G: Funktor[G]): Funktor[λ[α ⇒ (F[α], G[α])]] = new Funktor[λ[α ⇒ (F[α], G[α])]] {
    def lift[A, B](f: A ⇒ B): ((F[A], G[A])) ⇒ (F[B], G[B]) = {
      case (fa, ga) ⇒ (F.map(fa)(f), G.map(ga)(f))
    }
  }

  /*
  implicit def prodFunktor[F[_]: Funktor, G[_]: Funktor]: Funktor[λ[α ⇒ (F[α], G[α])]] = new Funktor[λ[α ⇒ (F[α], G[α])]] {
    def lift[A, B](f: A ⇒ B): ((F[A], G[A])) ⇒ (F[B], G[B]) = {
      case (fa, ga) ⇒ (implicitly[Funktor[F]].map(fa)(f), implicitly[Funktor[G]].map(ga)(f))
    }
  }
  */

  implicit def sumFunktor[F[_], G[_]](implicit F: Funktor[F], G: Funktor[G]): Funktor[λ[α ⇒ Either[F[α], G[α]]]] = new Funktor[λ[α ⇒ Either[F[α], G[α]]]] {
    def lift[A, B](f: A ⇒ B): Either[F[A], G[A]] ⇒ Either[F[B], G[B]] = {
      case Right(ga) ⇒ Right(G.map(ga)(f))
      case Left(fa) ⇒ Left(F.map(fa)(f))
    }
  }

  implicit def expFunktor[F[_], G[_]](implicit F: ContraFunktor[F], G: Funktor[G]): Funktor[λ[α ⇒ F[α] ⇒ G[α]]] = new Funktor[λ[α ⇒ F[α] ⇒ G[α]]] {
    def lift[A, B](f: A ⇒ B): (F[A] ⇒ G[A]) ⇒ F[B] ⇒ G[B] = ???
  }

  implicit def compFunktor[F[_], G[_]](implicit F: Funktor[F], G: Funktor[G]): Funktor[λ[α ⇒ F[G[α]]]] = new Funktor[λ[α ⇒ F[G[α]]]] {
    def lift[A, B](f: A ⇒ B): F[G[A]] ⇒ F[G[B]] = F.lift(G.lift(f))
  }

  implicit def functionFunktor[R, F[_]](implicit F: Funktor[F]): Funktor[λ[α ⇒ R ⇒ F[α]]] = new Funktor[λ[α ⇒ R ⇒ F[α]]] {
    def lift[A, B](f: A ⇒ B): (R ⇒ F[A]) ⇒ R ⇒ F[B] = ???
  }

}


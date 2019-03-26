package lesson10

import cats.Monoid
import lesson8.{ContraFunktor, Funktor}
import scala.language.higherKinds

trait Manoidal[F[_]] {
  def unit: F[Unit]

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}


trait Applikative[F[_]] extends Funktor[F] with Manoidal[F] {
  def pure[A](a: A): F[A] = as(unit, a)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) ⇒ C): F[C] = map(product(fa, fb))(ab ⇒ f(ab))
}


object Applikative {

  // universal functor derivation rules
  implicit val idApplikative: Applikative[λ[α ⇒ α]] = new Applikative[λ[α ⇒ α]] {
    def unit: Unit = Unit

    def product[A, B](fa: A, fb: B): (A, B) = (fa, fb)

    def lift[A, B](f: A ⇒ B): A ⇒ B = f
  }


  // including M = 1 (any singleton)
  implicit def constApplikative[M : Monoid]: Applikative[λ[α ⇒ M]] = new Applikative[λ[α ⇒ M]] {
    import cats.syntax.semigroup._

    def unit: M = implicitly[Monoid[M]].empty

    def product[A, B](fa: M, fb: M): M = fa |+| fb

    def lift[A, B](f: A ⇒ B): M ⇒ M = m ⇒ m
  }


  implicit def prodApplikative[F[_] : Applikative, G[_] : Applikative]: Applikative[λ[α ⇒ (F[α], G[α])]] = new Applikative[λ[α ⇒ (F[α], G[α])]] {
    def unit: (F[Unit], G[Unit]) = (implicitly[Applikative[F]].unit, implicitly[Applikative[G]].unit)

    def product[A, B](faga: (F[A], G[A]), fbgb: (F[B], G[B])): (F[(A, B)], G[(A, B)]) = {
      val fa = faga._1
      val ga = faga._2
      val fb = fbgb._1
      val gb = fbgb._2
      (implicitly[Applikative[F]].product(fa, fb), implicitly[Applikative[G]].product(ga, gb))
    }

    def lift[A, B](f: A ⇒ B): ((F[A], G[A])) ⇒ (F[B], G[B]) = { case(fa, ga) ⇒
      (implicitly[Applikative[F]].map(fa)(f), implicitly[Applikative[G]].map(ga)(f))
    }
  }


  implicit def compApplikative[F[_] : Applikative, G[_] : Applikative]: Applikative[λ[α ⇒ F[G[α]]]] = new Applikative[λ[α ⇒ F[G[α]]]] {
    def unit: F[G[Unit]] =
      implicitly[Applikative[F]].pure(implicitly[Applikative[G]].unit)

    def product[A, B](fga: F[G[A]], fgb: F[G[B]]): F[G[(A, B)]] =
      implicitly[Applikative[F]].map2(fga, fgb) { case(ga, gb) ⇒ implicitly[Applikative[G]].product(ga, gb)}

    def lift[A, B](f: A ⇒ B): F[G[A]] ⇒ F[G[B]] =
      fga ⇒ implicitly[Applikative[F]].map(fga)(ga ⇒ implicitly[Applikative[G]].map(ga)(f))
  }


  // CANNOT BE DERIVED IN GENERAL
  implicit def sumApplikative[F[_] : Applikative, G[_] : Applikative]: Applikative[λ[α ⇒ F[α] Either G[α]]] = ???


  // BUT THESE ONES CAN BE
  implicit def someSumApplikative[F[_] : Applikative, M : Monoid]: Applikative[λ[α ⇒ M Either F[α]]] = new Applikative[λ[α ⇒ M Either F[α]]] {
    def unit: Either[M, F[Unit]] = Right(implicitly[Applikative[F]].unit)

    import cats.syntax.semigroup._
    def product[A, B](mfa: Either[M, F[A]], mfb: Either[M, F[B]]): Either[M, F[(A, B)]] = {
      (mfa, mfb) match {
        case (Right(fa), Right(fb)) ⇒ Right(implicitly[Applikative[F]].product(fa, fb))
        case (Left(ml), Left(mr)) ⇒ Left(ml |+| mr)
        case (Left(m), _) ⇒ Left(m)
        case (_, Left(m)) ⇒ Left(m)
      }
    }

    def lift[A, B](f: A ⇒ B): Either[M, F[A]] ⇒ Either[M, F[B]] = {
      case Right(fa) ⇒ Right(implicitly[Applikative[F]].map(fa)(f))
      case Left(m) ⇒ Left(m)
    }
  }


  implicit def pointedApplikative[F[_] : Applikative]: Applikative[λ[α ⇒ α Either F[α]]] = new Applikative[λ[α ⇒ α Either F[α]]] {
    def unit: Either[Unit, F[Unit]] = Right(implicitly[Applikative[F]].unit)

    def product[A, B](afa: Either[A, F[A]], bfb: Either[B, F[B]]): Either[(A, B), F[(A, B)]] = {
      (afa, bfb) match {
        case (Right(fa), Right(fb)) ⇒ Right(implicitly[Applikative[F]].product(fa, fb))
        case (Left(a), Left(b)) ⇒ Left((a, b))
        case (Left(a), Right(fb)) ⇒
          Right(implicitly[Applikative[F]].product(implicitly[Applikative[F]].pure(a),fb))
        case (Right(fa), Left(b)) ⇒
          Right(implicitly[Applikative[F]].product(fa, implicitly[Applikative[F]].pure(b)))
      }
    }

    def lift[A, B](f: A ⇒ B): Either[A, F[A]] ⇒ Either[B, F[B]] = {
      case Right(fa) ⇒ Right(implicitly[Applikative[F]].map(fa)(f))
      case Left(a) ⇒ Left(f(a))
    }
  }


  // TODO
  implicit def freemApplikative[F[_] : Funktor]: Applikative[λ[α ⇒ Free[F, α]]] = new Applikative[λ[α ⇒ Free[F, α]]] {
    def unit: Free[F, Unit] = ???
    def product[A, B](fa: Free[F, A], fb: Free[F, B]): Free[F, (A, B)] = ???
    def lift[A, B](f: A ⇒ B): Free[F, A] ⇒ Free[F, B] = ???
  }


  implicit def someExpApplikative[F[_] : ContrApplikative]: Applikative[λ[α ⇒ F[α] ⇒ α]] = new Applikative[λ[α ⇒ F[α] ⇒ α]] {
    def unit: F[Unit] ⇒ Unit = _ ⇒ ()

    def product[A, B](faa: F[A] ⇒ A, fbb: F[B] ⇒ B): F[(A, B)] ⇒ (A, B) = ???

    def lift[A, B](f: A ⇒ B): (F[A] ⇒ A) ⇒ F[B] ⇒ B = faa ⇒ {
      fb ⇒ {
        val fa = implicitly[ContrApplikative[F]].contramap(fb)(f)
        val a = faa(fa)
        f(a)
      }
    }
  }
}


trait ContrApplikative[F[_]] extends ContraFunktor[F] with Manoidal[F] {
  def conquer[A]: F[A] = contramap[Unit, A](unit)((_: A) ⇒ ())

  def contramap2[A, B, C](fa: F[A], fb: F[B])(f: C ⇒ (A, B)): F[C] = contramap(product(fa, fb))(f)
}


object ContrAplikative {

  // including M = 1 (any singleton)
  implicit def constContrApplikative[M : Monoid]: ContrApplikative[λ[α ⇒ M]] = new ContrApplikative[λ[α ⇒ M]] {
    import cats.syntax.semigroup._

    def unit: M = implicitly[Monoid[M]].empty

    def product[A, B](fa: M, fb: M): M = fa |+| fb

    def contralift[A, B](f: B ⇒ A): M ⇒ M = m ⇒ m
  }


  implicit def prodContrApplikative[F[_] : ContrApplikative, G[_] : ContrApplikative]: ContrApplikative[λ[α ⇒ (F[α], G[α])]] = new ContrApplikative[λ[α ⇒ (F[α], G[α])]] {
    def unit: (F[Unit], G[Unit]) = (implicitly[ContrApplikative[F]].unit, implicitly[ContrApplikative[G]].unit)

    def product[A, B](faga: (F[A], G[A]), fbgb: (F[B], G[B])): (F[(A, B)], G[(A, B)]) = {
      val fa = faga._1
      val ga = faga._2
      val fb = fbgb._1
      val gb = fbgb._2
      (implicitly[ContrApplikative[F]].product(fa, fb), implicitly[ContrApplikative[G]].product(ga, gb))
    }

    def contralift[A, B](f: B ⇒ A): ((F[A], G[A])) ⇒ (F[B], G[B]) = { case (fa, ga) ⇒
      (implicitly[ContrApplikative[F]].contramap(fa)(f), implicitly[ContrApplikative[G]].contramap(ga)(f))
    }
  }


  // CANNOT BE DERIVED IN GENERAL
  implicit def sumContrApplikative[F[_] : ContrApplikative, G[_] : ContrApplikative]: ContrApplikative[λ[α ⇒ F[α] Either G[α]]] = ???


  implicit def compContrApplikative[F[_] : Applikative, G[_] : ContrApplikative]: ContrApplikative[λ[α ⇒ F[G[α]]]] = new ContrApplikative[λ[α ⇒ F[G[α]]]] {
    def unit: F[G[Unit]] = implicitly[Applikative[F]].pure(implicitly[ContrApplikative[G]].unit)

    def product[A, B](fga: F[G[A]], fgb: F[G[B]]): F[G[(A, B)]] =
      implicitly[Applikative[F]].map2(fga, fgb) { case (ga, gb) ⇒ implicitly[ContrApplikative[G]].product(ga, gb)}

    def contralift[A, B](f: B ⇒ A): F[G[A]] ⇒ F[G[B]] =
      fga ⇒ implicitly[Applikative[F]].map(fga)(ga ⇒ implicitly[ContrApplikative[G]].contramap(ga)(f))

  }


  implicit def expContrApplikative[F[_] : Funktor, G[_] : ContrApplikative]: ContrApplikative[λ[α ⇒ F[α] ⇒ G[α]]] = new ContrApplikative[λ[α ⇒ F[α] ⇒ G[α]]] {
    def unit: F[Unit] ⇒ G[Unit] = _ ⇒ implicitly[ContrApplikative[G]].unit

    def product[A, B](faga: F[A] ⇒ G[A], fbgb: F[B] ⇒ G[B]): F[(A, B)] ⇒ G[(A, B)] = fab ⇒ {
      val fa = implicitly[Funktor[F]].map(fab) { case (a, _) ⇒ a}
      val fb = implicitly[Funktor[F]].map(fab) { case (_, b) ⇒ b}
      val ga = faga(fa)
      val gb = fbgb(fb)
      implicitly[ContrApplikative[G]].product(ga, gb)
    }

    def contralift[A, B](f: B ⇒ A): (F[A] ⇒ G[A]) ⇒ F[B] ⇒ G[B] = faga ⇒ { fb ⇒
      val fa = implicitly[Funktor[F]].map(fb)(f)
      val ga = faga(fa)
      implicitly[ContrApplikative[G]].contramap(ga)(f)
    }
  }
}

case class Free[F[_], A](unwrap: A Either F[Free[F, A]])
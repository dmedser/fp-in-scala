package lesson8

trait Funktor[F[_]] {
  def lift[A, B](f: A => B): F[A] => F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = lift(f)(fa)

  def void[A](fa: F[A]): F[Unit] = map(fa)(_ => ())

  def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)

  def widen[A, B >: A](fa: F[A]): F[B] = map(fa)(v => v: B)

  def tupleLeft[A, B](fa: F[A], b: B): F[(B, A)] = map(fa)(a => (b, a))

  def tupleRight[A, B](fa: F[A], b: B): F[(A, B)] = map(fa)(a => (a, b))

  def fproduct[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa)(a => (a, f(a)))
}

trait ContraFunktor[F[_]] {
  def contralift[A, B](f: B => A): F[A] => F[B]

  def contramap[A, B](fa: F[A])(f: B => A): F[B] = contralift(f)(fa)
}

object Funktor {
  def apply[F[_]: Funktor]: Funktor[F] = implicitly

  // concrete functor examples
  implicit val optionFunktor: Funktor[Option] = new Funktor[Option] {
    def lift[A, B](f: A => B): Option[A] => Option[B] = {
      case Some(a) => Some(f(a))
      case None => None
    }
  }

  implicit val listFunktor: Funktor[List] = new Funktor[List] {
    def lift[A, B](f: A => B): List[A] => List[B] = list => list.map(f)
  }

  // universal functor derivation rules
  implicit val idFunktor: Funktor[λ[α => α]] = new Funktor[λ[α => α]] {
    def lift[A, B](f: A => B): A => B = f
  }

  implicit def constFunktor[C]: Funktor[λ[α => C]] = new Funktor[λ[α => C]] {
    def lift[A, B](f: A => B): C => C = c => c
  }

  implicit def prodFunktor[F[_], G[_]](implicit F: Funktor[F], G: Funktor[G]): Funktor[λ[α => (F[α], G[α])]] =
    new Funktor[λ[α => (F[α], G[α])]] {
      def lift[A, B](f: A => B): ((F[A], G[A])) => (F[B], G[B]) = {
        case (fa, ga) => (F.map(fa)(f), G.map(ga)(f))
      }
    }

  implicit def sumFunktor[F[_], G[_]](implicit F: Funktor[F], G: Funktor[G]): Funktor[λ[α => F[α] Either G[α]]] =
    new Funktor[λ[α => F[α] Either G[α]]] {
      def lift[A, B](f: A => B): Either[F[A], G[A]] => Either[F[B], G[B]] = {
        case Right(ga) => Right(G.map(ga)(f))
        case Left(fa) => Left(F.map(fa)(f))
      }
    }

  implicit def expFunktor[F[_], G[_]](implicit F: ContraFunktor[F], G: Funktor[G]): Funktor[λ[α => F[α] => G[α]]] =
    new Funktor[λ[α => F[α] => G[α]]] {
      def lift[A, B](f: A => B): (F[A] => G[A]) => F[B] => G[B] = { faga => fb =>
        {
          val fa = F.contramap(fb)(f)
          val ga = faga(fa)
          G.map(ga)(f)
        }
      }
    }

  implicit def compFunktor[F[_], G[_]](implicit F: Funktor[F], G: Funktor[G]): Funktor[λ[α => F[G[α]]]] =
    new Funktor[λ[α => F[G[α]]]] {
      def lift[A, B](f: A => B): F[G[A]] => F[G[B]] = F.lift(G.lift(f))
    }

  implicit def functionFunktor[R, F[_]](implicit F: Funktor[F]): Funktor[λ[α => R => F[α]]] =
    new Funktor[λ[α => R => F[α]]] {
      def lift[A, B](f: A => B): (R => F[A]) => R => F[B] =
        rfa =>
          r => {
            val fa = rfa(r)
            F.map(fa)(f)
        }
    }
}

object ContraFunktor {
  // similar rules for contravariant functors
  implicit def constContraFunktor[C]: ContraFunktor[λ[α => C]] = new ContraFunktor[λ[α => C]] {
    def contralift[A, B](f: B => A): C => C = c => c
  }

  implicit def prodContraFunktor[F[_], G[_]](
    implicit F: ContraFunktor[F],
    G: ContraFunktor[G]
  ): ContraFunktor[λ[α => (F[α], G[α])]] = new ContraFunktor[λ[α => (F[α], G[α])]] {
    def contralift[A, B](f: B => A): ((F[A], G[A])) => (F[B], G[B]) = {
      case (fa, ga) => (F.contramap(fa)(f), G.contramap(ga)(f))
    }
  }

  implicit def sumContraFunktor[F[_], G[_]](
    implicit F: ContraFunktor[F],
    G: ContraFunktor[G]
  ): ContraFunktor[λ[α => F[α] Either G[α]]] = new ContraFunktor[λ[α => F[α] Either G[α]]] {
    def contralift[A, B](f: B => A): Either[F[A], G[A]] => Either[F[B], G[B]] = {
      case Right(ga) => Right(G.contramap(ga)(f))
      case Left(fa) => Left(F.contramap(fa)(f))
    }
  }

  implicit def expContraFunktor[F[_], G[_]](
    implicit F: Funktor[F],
    G: ContraFunktor[G]
  ): ContraFunktor[λ[α => F[α] => G[α]]] = new ContraFunktor[λ[α => F[α] => G[α]]] {
    def contralift[A, B](f: B => A): (F[A] => G[A]) => F[B] => G[B] = faga => { fb =>
      {
        val fa = F.map(fb)(f)
        val ga = faga(fa)
        G.contramap(ga)(f)
      }
    }
  }

  implicit def compContraFunktor[F[_], G[_]](
    implicit F: Funktor[F],
    G: ContraFunktor[G]
  ): ContraFunktor[λ[α => F[G[α]]]] = new ContraFunktor[λ[α => F[G[α]]]] {
    def contralift[A, B](f: B => A): F[G[A]] => F[G[B]] = fga => F.map(fga)(ga => G.contramap(ga)(f))
  }

  implicit def functionContraFunktor[R, F[_]](implicit F: ContraFunktor[F]): ContraFunktor[λ[α => R => F[α]]] =
    new ContraFunktor[λ[α => R => F[α]]] {
      def contralift[A, B](f: B => A): (R => F[A]) => R => F[B] =
        rfa =>
          r => {
            val fa = rfa(r)
            F.contramap(fa)(f)
        }
    }
}

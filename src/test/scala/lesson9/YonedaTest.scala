package lesson9

import cats.{~>, Functor}
import cats.syntax.functor._
import cats.instances.option._
import org.scalatest.{MustMatchers, WordSpec}

class YonedaTest extends WordSpec with MustMatchers  {

  case class Maybe[A](opt: Option[A]) {
    println(s"[debug] new $this")
  }
  object Maybe {
    implicit val functorMaybe: Functor[Maybe] = new Functor[Maybe] {
      def map[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = Maybe(fa.opt.map(f))
    }
  }

  "Yoneda embedding" must {
    "just work" in {
      println("Creating Maybe...")
      val maybe = Maybe(Some(42))
      println(s"res = $maybe")

      println("Direct mapping on Maybe...")
      val maybeMapped = maybe.map(_.toString).map(_ + " is the answer").map(_.toUpperCase)
      println(s"res = $maybeMapped")

      println("Lazy mapping on Yoneda[Maybe] (needs a functor)...")
      val maybeLazyMapped = Yoneda.lift(maybe).map(_.toString).map(_ + " is the answer").map(_.toUpperCase).lower
      println(s"res = $maybeLazyMapped")
    }
  }


  case class Perhaps[A](opt: Option[A]) {
    println(s"[debug] new $this")
  }

  "Coyoneda embedding" must {
    "just work" in {
      println("Creating Perhaps...")
      val perhaps = Perhaps(Some(42))
      println(s"res = $perhaps")

      println("Free mapping on Coyoneda[Perhaps] (doesn't need a functor)...")
      val perhapsFreeMapped = Coyoneda.lift(perhaps).map(_.toString).map(_ + " is the answer").map(_.toUpperCase)
      println(s"res = $perhapsFreeMapped")

      val functorPerhaps: Functor[Perhaps] = new Functor[Perhaps] {
        def map[A, B](fa: Perhaps[A])(f: A => B): Perhaps[B] = Perhaps(fa.opt.map(f))
      }

      println("Lowering Coyoneda[Perhaps] (now needs a functor)...")
      val perhapsFreeMappedLowered = perhapsFreeMapped.lower(functorPerhaps)
      println(s"res = $perhapsFreeMappedLowered")

      println("FoldMapping Coyoneda[Perhaps] into another functor")
      val perhapsFoldMapped = perhapsFreeMapped.foldMap(λ[Perhaps ~> Option](_.opt))
      println(s"res = $perhapsFoldMapped")
    }
  }


}
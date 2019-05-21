package lesson15

import cats.Functor
import cats.syntax.functor._
import monocle.Iso

object Folds extends App {

  // Many ADTs have similar `fold` functions
  val opt: Option[Int] = Option(4)
  val optFoldStr: String = opt.fold("empty")(x => s"$x")
  val optFoldBool: Boolean = opt.fold(false)(_ => true)
  val optFoldOpt: Option[Int] = opt.fold(Option.empty[Int])(x => Some(x))

  val list: List[Int] = List(1, 2, 3, 4)
  val listFoldStr: String = list.foldRight("")((x, s) => s"$x$s")
  val listFoldBool: Boolean = list.foldRight(true)((x, s) => (x > 0) && s)
  val listFoldList: List[Int] = list.foldRight(List.empty[Int])(_ :: _)

  val eith: Either[String, Int] = Right(4)
  val eithFoldStr: String = eith.fold(l => s"error: $l", r => s"value: $r")
  val eithFoldBool: Boolean = eith.fold(_ => false, _ => true)
  val eithFoldEith: Either[String, Int] = eith.fold(Left(_), Right(_))

  // case constructors used in folds can reconstruct the same ADT

  // Let's declare a (recursive) ADT
  sealed trait Adt
  object Adt {
    case class Case1(a: Int, b: String) extends Adt
    case class Case2(a: Int, b: Adt, c: Adt) extends Adt
    case object Case3 extends Adt
  }
  import Adt._

  // How would one write `fold`?
  def foldAdt[R](x: Adt)(f1: (Int, String) => R, f2: (Int, R, R) => R, f3: R): R = x match {
    case Case1(a, b) => f1(a, b)
    case Case2(a, b, c) => f2(a, foldAdt(b)(f1, f2, f3), foldAdt(c)(f1, f2, f3))
    case Case3 => f3
  }

  // Let's make a blockbox container wrapping an Adt instance. We cannot
  // access it directly, but still can reconstruct
  // or interpret in any other way.
  case class AdtBlackBox(private val x: Adt) {
    def fold[R](f1: (Int, String) => R, f2: (Int, R, R) => R, f3: R): R = x match {
      case Case1(a, b) => f1(a, b)
      case Case2(a, b, c) => f2(a, AdtBlackBox(b).fold(f1, f2, f3), AdtBlackBox(c).fold(f1, f2, f3))
      case Case3 => f3
    }
  }

  // What's (f1, f2, f3)? A record of functions - can be expressed as a trait.
  trait AdtDict[R] {
    def case1(a: Int, b: String): R
    def case2(a: Int, b: R, c: R): R
    def case3: R
  }
  // Sum of cases becomes a product of functions

  // Let's express `fold` via Dict
  case class AdtBlackBox2(private val x: Adt) {
    def fold[R](dict: AdtDict[R]): R = x match {
      case Case1(a, b) => dict.case1(a, b)
      case Case2(a, b, c) => dict.case2(a, AdtBlackBox2(b).fold(dict), AdtBlackBox2(c).fold(dict))
      case Case3 => dict.case3
    }
  }

  // Dict is called final tagless algebra
  type AdtAlgebraTF[R] = AdtDict[R]

  trait AdtInitialAlgebraTF {
    def fold[R](alg: AdtAlgebraTF[R]): R
  }

  case class AdtBlackBox3(private val x: Adt) extends AdtInitialAlgebraTF {
    def fold[R](alg: AdtAlgebraTF[R]): R = x match {
      case Case1(a, b) => alg.case1(a, b)
      case Case2(a, b, c) => alg.case2(a, AdtBlackBox3(b).fold(alg), AdtBlackBox3(c).fold(alg))
      case Case3 => alg.case3
    }
  }

  // Boehm-Berarducci encoding of ADTs as rank-2 types
  // http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html#encoded
  trait Initial[-Alg[_]] {
    def fold[R](a: Alg[R]): R
  }

  type AdtInitial = Initial[AdtAlgebraTF]

  object AdtCons extends AdtAlgebraTF[AdtInitial] {
    def case1(a: Int, b: String): AdtInitial = new Initial[AdtAlgebraTF] {
      def fold[R](alg: AdtAlgebraTF[R]): R = alg.case1(a, b)
    }
    def case2(a: Int, b: AdtInitial, c: AdtInitial): AdtInitial = new Initial[AdtAlgebraTF] {
      def fold[R](alg: AdtAlgebraTF[R]): R = alg.case2(a, b.fold(alg), c.fold(alg))
    }
    def case3: AdtInitial = new Initial[AdtAlgebraTF] { def fold[R](alg: AdtAlgebraTF[R]): R = alg.case3 }
  }

  // Any ADT can be presented in Boehm-Berarducci encoding
  val iso: Iso[Adt, Initial[AdtAlgebraTF]] = Iso[Adt, Initial[AdtAlgebraTF]](
    adt =>
      new Initial[AdtAlgebraTF] {
        def fold[R](alg: AdtAlgebraTF[R]): R = {
          def go(x: Adt): R = x match {
            case Case1(a, b) => alg.case1(a, b)
            case Case2(a, b, c) => alg.case2(a, go(b), go(c))
            case Case3 => alg.case3
          }
          go(adt)
        }
      }
  )(_.fold[Adt](new AdtAlgebraTF[Adt] {
    def case1(a: Int, b: String): Adt = Case1(a, b)
    def case2(a: Int, b: Adt, c: Adt): Adt = Case2(a, b, c)
    def case3: Adt = Case3
  }))

  val adt1 = Case2(55, Case3, Case1(1, "x"))
  println(adt1)

  val adt1x = iso.reverseGet(iso.get(adt1))
  println(adt1x)

  val adt2bb = {
    import AdtCons._
    case2(55, case3, case1(1, "x"))
  }
  val adt2bbx = iso.get(iso.reverseGet(adt2bb))

  val adtStringAlg: AdtAlgebraTF[String] = new AdtDict[String] {
    def case1(a: Int, b: String): String = s"Case1($a,$b)"
    def case2(a: Int, b: String, c: String): String = s"Case2($a,$b,$c)"
    def case3: String = "Case3"
  }
  println(adt2bb.fold(adtStringAlg))
  println(adt2bbx.fold(adtStringAlg))

  // F-algebras

  // ADT may be expressed as a functor
  sealed trait AdtF[A]
  object AdtF {
    case class Case1F[A](a: Int, b: String) extends AdtF[A]
    case class Case2F[A](a: Int, b: A, c: A) extends AdtF[A]
    case class Case3F[A]() extends AdtF[A]
  }
  import AdtF._

  implicit val adtFunctor: Functor[AdtF] = new Functor[AdtF] {
    def map[A, B](fa: AdtF[A])(f: A => B): AdtF[B] = fa match {
      case Case1F(a, b) => Case1F(a, b)
      case Case2F(a, b, c) => Case2F(a, f(b), f(c))
      case Case3F() => Case3F()
    }
  }

  type FAlgebra[F[_], A] = F[A] => A
  type FCoalgebra[F[_], A] = A => F[A]

  trait AdtFInitialAlgebra {
    def fold[R](alg: FAlgebra[AdtF, R]): R
  }

  // Recursive fixed point
  case class Fix[F[_]](unfix: F[Fix[F]])

  // Mu - least fixed point for F
  trait Mu[F[_]] {
    def fold[R](alg: FAlgebra[F, R]): R
  }
  object Mu {
    type Arbitrary
    def apply[F[_]](f: FAlgebra[F, Arbitrary] => Arbitrary): Mu[F] = new Mu[F] {
      override def fold[R](alg: FAlgebra[F, R]): R = f(alg.asInstanceOf[FAlgebra[F, Arbitrary]]).asInstanceOf[R]
    }
  }

  // Nu - greatest fixed point for F
  trait Nu[F[_]] {
    type A
    def a: A
    def coalg: FCoalgebra[F, A]
  }
  object Nu {
    def apply[F[_], Z](z: Z, calg: FCoalgebra[F, Z]): Nu[F] = new Nu[F] {
      type A = Z
      val a: A = z
      val coalg: FCoalgebra[F, Z] = calg
    }
  }

  def isoFixMu[F[_] : Functor]: Iso[Fix[F], Mu[F]] =
    Iso[Fix[F], Mu[F]](
      fix =>
        Mu { alg =>
          def go(fx: Fix[F]): Mu.Arbitrary = alg(fx.unfix.map(go))
          go(fix)
        }
    )(_.fold[Fix[F]](Fix(_)))

  def isoFixNu[F[_] : Functor]: Iso[Fix[F], Nu[F]] =
    Iso[Fix[F], Nu[F]](Nu[F, Fix[F]](_, _.unfix))(nu => {
      def go(x: nu.A): Fix[F] = Fix[F](nu.coalg(x).map(go))
      go(nu.a)
    })

  def cata[F[_] : Functor, R](alg: FAlgebra[F, R])(fa: Fix[F]): R = alg(fa.unfix.map(cata(alg)))
  def ana[F[_] : Functor, R](coalg: FCoalgebra[F, R])(a: R): Fix[F] = Fix(coalg(a).map(ana(coalg)))

  def cataMu[F[_] : Functor, R](alg: FAlgebra[F, R])(fa: Mu[F]): R = fa.fold(alg)
  def anaNu[F[_] : Functor, R](coalg: FCoalgebra[F, R])(a: R): Nu[F] = Nu(a, coalg)

  case class AdtBlackBox4(private val x: Fix[AdtF]) extends AdtFInitialAlgebra {
    def fold[R](alg: FAlgebra[AdtF, R]): R = alg(x.unfix.map(y => AdtBlackBox4(y).fold(alg)))
  }

  val adtStringFAlg: FAlgebra[AdtF, String] = {
    case Case1F(a, b) => s"Case1($a,$b)"
    case Case2F(a, b, c) => s"Case2($a,$b,$c)"
    case Case3F() => s"Case3"
  }

  val adt3: Fix[AdtF] = Fix(Case2F(55, Fix(Case3F()), Fix(Case1F(1, "x"))))
  println(cata(adtStringFAlg)(adt3))

  val isoFixMuAdt = isoFixMu[AdtF]
  val isoFixNuAdt = isoFixNu[AdtF]

  println(isoFixMuAdt.reverseGet(isoFixMuAdt.get(adt3)))
  println(isoFixNuAdt.reverseGet(isoFixNuAdt.get(adt3)))

  isoFixMuAdt.get(adt3).fold[String](adtStringFAlg)

}
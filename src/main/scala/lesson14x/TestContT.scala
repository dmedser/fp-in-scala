package lesson14x

object TestContT extends App {
  import cats.data._
  import cats.implicits._
  import cats._

  def x: ContT[Eval, String, Int] = ContT.pure(5)

  def f(i: Int): ContT[Eval, String, String] = ContT[Eval, String, String] { cb =>
    val l1: String = s"${i * 3}"
    val l2: String = s"${i * 5}"
    (cb(l1), cb(l2)).mapN((x,y) => s"f:\ncb1=$x\ncb2=$y")
  }

  def g(l: String): ContT[Eval, String, String] = ContT[Eval, String, String] { cb =>
    val ll = s"$l + 2"
    val s1: String = ll.toString
    val s2 = s1.reverse
    (cb(s1), cb(s2)).mapN((x,y) => s"g(cb1=$x; cb2=$y)")
  }


  def callCC[F[_], R, A, B](k: (A => ContT[F, R, B]) => ContT[F, R, A]): ContT[F, R, A] =
    ContT(c => k(x => ContT(_ => c(x))).run(c))


  def test: ContT[Eval, String, String] = callCC[Eval, String, String, Any] { exit =>
    for {
      i <- x
      j <- f(i)
      _ <- if (i > 3) exit(s"exit with $j") else Monad[ContT[Eval, String, ?]].unit
      k <- g(j)
    } yield k
  }

  println(test.run(Eval.now).value)
}
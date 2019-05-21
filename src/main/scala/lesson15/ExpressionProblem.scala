package lesson15

object ExpressionProblem1 extends App {

  sealed trait Expr
  case class Lit(i: Int) extends Expr
  case class Neg(e: Expr) extends Expr
  case class Add(a: Expr, b: Expr) extends Expr

  val e = Add(Lit(9), Neg(Add(Lit(5), Lit(3))))
  // 9 - (5 + 3)

  def expr2int(e: Expr): Int = e match {
    case Lit(i) => i
    case Neg(a) => -expr2int(a)
    case Add(a, b) => expr2int(a) + expr2int(b)
  }

  expr2int(e) // 1

  def expr2str(e: Expr): String = e match {
    case Lit(i) => s"$i"
    case Neg(a) => s"-${expr2str(a)}"
    case Add(a, b) => s"(${expr2str(a)} + ${expr2str(b)})"
  }

  expr2str(e) // 9 + -(5 + 3)
}

object ExpressionProblem2 extends App {
  type Repr = Int
  def lit(i: Int): Repr = i
  def neg(a: Repr): Repr = -a
  def add(a: Repr, b: Repr): Repr = a + b

  val e: Repr = add(lit(9), neg(add(lit(5), lit(3)))) // 1
}

object ExpressionProblem3 extends App {
  type Repr = String
  def lit(i: Int): Repr = s"$i"
  def neg(a: Repr): Repr = s"-$a"
  def add(a: Repr, b: Repr): Repr = s"($a + $b)"

  val e: Repr = add(lit(9), neg(add(lit(5), lit(3)))) // 9 + -(5 + 3)
}

object ExpressionProblem4 extends App {

  trait ExprDict[Repr] {
    def lit(i: Int): Repr
    def neg(a: Repr): Repr
    def add(a: Repr, b: Repr): Repr
  }

  def e[Repr](implicit ev: ExprDict[Repr]): Repr = {
    import ev._
    add(lit(9), neg(add(lit(5), lit(3))))
  }

  implicit val exprDictInt = new ExprDict[Int] {
    def lit(i: Int): Int = i
    def neg(a: Int): Int = -a
    def add(a: Int, b: Int): Int = a + b
  }

  implicit val exprDictStr = new ExprDict[String] {
    def lit(i: Int): String = s"$i"
    def neg(a: String): String = s"-$a"
    def add(a: String, b: String): String = s"($a + $b)"
  }

  e[Int] // 1
  e[String] // 9 + -(5 + 3)

  // type ExprTF = forall R. given ExprDict[R] => R
  trait ExprTF1 {
    def apply[Repr : ExprDict]: Repr
  }

  trait ExprTF2 {
    def apply[Repr](fa: ExprDict[Repr]): Repr
  }

  trait ExprTF3 {
    def apply[R]: (Int => R, R => R, (R, R) => R) => R
  }

  trait TF[-F[_]] {
    def apply[R](fr: F[R]): R
  }
  type ExprTF = TF[ExprDict]

}
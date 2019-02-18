package lesson4

object TypesOfImplicits {

  case class C(x: Int)

  def implicitValues(): Unit = {

    case class C(x: Int)

    implicit val x: C = C(1)
  }

  def implicitFunctions(): Unit = {


    implicit def f: C = C(1)
  }
}

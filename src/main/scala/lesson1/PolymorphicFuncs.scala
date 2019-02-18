package lesson1

object PolymorphicFuncs {

  case class Cat(name: String)

  def listOfDuplicates[A](x: A, length: Int): List[A] = {
    if (length < 1)
      Nil
    else
      x :: listOfDuplicates(x, length - 1)
  }

}

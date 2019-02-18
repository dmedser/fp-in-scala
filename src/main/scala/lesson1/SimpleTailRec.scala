package lesson1

object SimpleTailRec {

  def unsafeFib(n: Int): Int = n match {
    case 0 => n
    case 1 => n
    case _ => unsafeFib(n - 1) + unsafeFib(n - 2)
  }

  def safeFib(n: Int): Int = {
    @annotation.tailrec
    def go(x: Int, y: Int, acc: Int): Int =
      if (acc == n) y else go(y, x + y, acc + 1)

    go(1, 1, 1)
  }

}

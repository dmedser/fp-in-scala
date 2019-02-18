package lesson3

sealed trait Maybe[+A] {
  def get: A
  def map[B](f: A => B): Maybe[B] = this match {
    case Just(a) => Just(f(a))
    case Empty => Empty
  }
  def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match {
    case Just(a) => f(a)
    case Empty => Empty
  }
}
case class Just[+A](value: A) extends Maybe[A] {
  override def get: A = value
}
case object Empty extends Maybe[Nothing] {
  override def get = throw new Exception("null")
}

object Maybe {
  def apply[A](value: A): Maybe[A] =
    if (value == null) Empty
    else Just(value)
}

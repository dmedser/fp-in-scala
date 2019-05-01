package lesson2

import scala.annotation.tailrec

sealed trait MyLinkedList[+A] {
  def head: A
  def tail: MyLinkedList[A]

  def map[R](f: A => R): MyLinkedList[R] = this match {
    case Nil => Nil
    case Cons(head, tail) => Cons(f(head), tail.map(f))
  }

  def drop(n: Int): MyLinkedList[A] = {
    @tailrec
    def go(res: MyLinkedList[A], acc: Int): MyLinkedList[A] =
      if (acc == n) res
      else go(res.tail, acc + 1)

    if (n <= this.size - 1)
      go(this, 0)
    else
      Nil
  }

  def dropWhile(f: A => Boolean): MyLinkedList[A] = {
    @tailrec
    def go(res: MyLinkedList[A]): MyLinkedList[A] =
      res match {
        case Cons(head, tail) if f(head) => go(tail)
        case _ => res
      }

    go(this)
  }

  def size: Int = {
    @tailrec
    def go(res: MyLinkedList[A], size: Int): Int = res match {
      case Nil => size
      case Cons(_, tail) => go(tail, size + 1)
    }

    go(this, 0)
  }

  def append[B >: A](a: MyLinkedList[B]): MyLinkedList[B] = {
    def go(res: MyLinkedList[B]): MyLinkedList[B] =
      res match {
        case Nil => a
        case Cons(head, tail) => Cons(head, go(tail))
      }

    go(this)
  }

  def +++[B >: A](a: MyLinkedList[B]): MyLinkedList[B] = append(a)


  override def toString: String = s"MyLinkedList($makeStr)"

  private def makeStr: String = this match {
    case Nil => ""
    case Cons(head, tail) =>
      head.toString + (tail match {
        case Nil => ""
        case Cons(_, _) => ", " + tail.makeStr
      })
  }

  /**
    * Implement function returning a List consisting of all but the last element of a List
    */
  //def init: List[A] = ???
  def init: MyLinkedList[A] = {
    def go(res: MyLinkedList[A]): MyLinkedList[A] = res match {
      case Cons(head, tail) if tail != Nil => Cons(head, go(tail))
      case _ => Nil
    }
    go(this)
  }


  /**
    * Implement function that folds list starting from first element by applying f function
    */
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(res: B, ys: MyLinkedList[A]): B = ys match {
      case Cons(head, tail) => go(f(res, head), tail)
      case _ => res
    }
    go(z, this)
  }

  /**
    * Implement using foldLeft
    */
  def foldRight[B](z: B)(f: (A, B) => B): B = {
    def go(res: B, ys: MyLinkedList[A]): B = ys match {
      case Cons(head, tail) => f(head, go(res, tail))
      case _ => res
    }
    go(z, this)
  }

  /**
    * Implement using foldRight
    *
    * @return length of list
    */
  def length: Int = {
    this.foldRight(0)((_, acc) => acc + 1)
  }

  /**
    *
    * Implement function returning the reverse of a list (given List(1,2,3) it returns List(3,2,1) )
    */
  //def reverse: List[A] = ???
  def reverse: MyLinkedList[A] = {
    @tailrec
    def go(l: MyLinkedList[A], r: MyLinkedList[A]): MyLinkedList[A] = l match {
      case Cons(head, tail) => go(tail, Cons(head, r))
      case _ => r
    }
    go(this, Nil)
  }

  /**
    * Write a function that concatenates a list of lists into a single list. Its runtime
    * should be linear in the total length of all lists. Try to use functions we have already
    * defined.
    *
    */
  //def flatten: List[A] = ???
  def flatten: MyLinkedList[A] = {
    this.foldLeft(MyLinkedList[A]()){(acc, elem) => elem match {
      case list: MyLinkedList[A] => acc append list
      case value => acc append Cons(value, Nil)
    }}
  }

  /**
    * Implement flatMap function
    * For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in
    * List(1,1,2,2,3,3) .
    *
    */
  //def flatMap[B](f: A => List[B]): List[B] = ???
  def flatMap[B](f: A => MyLinkedList[B]): MyLinkedList[B] = this match {
    case Nil => Nil
    case Cons(head, tail) => f(head) append tail.flatMap(f)
  }

}

case object Nil extends MyLinkedList[Nothing] {
  override def head = throw new Exception("nil")
  override def tail = throw new UnsupportedOperationException("tail")
}
case class Cons[A](head: A, tail: MyLinkedList[A]) extends MyLinkedList[A]

object MyLinkedList {
  def apply[A](as: A*): MyLinkedList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: MyLinkedList[A], a2: MyLinkedList[A]): MyLinkedList[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
}
package lesson11

import lesson10.Applikative

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  implicit val treeInorderDFSTraversable: Traversable[Tree] = new Traversable[Tree] {
    def traverse[G[_] : Applikative, A, B](tree: Tree[A])(f: A => G[B]): G[Tree[B]] = tree match {
      case Branch(value, left, right) =>
        Applikative[G].map3(traverse(left)(f), f(value), traverse(right)(f)) { case (lb, b, rb) => Branch(b, lb, rb) }
      case Leaf(value) => Applikative[G].map(f(value))(Leaf(_))
    }
  }

  implicit val treePreorderDFSTraversable: Traversable[Tree] = new Traversable[Tree] {
    def traverse[G[_] : Applikative, A, B](tree: Tree[A])(f: A => G[B]): G[Tree[B]] = tree match {
      case Branch(value, left, right) =>
        Applikative[G].map3(f(value), traverse(left)(f), traverse(right)(f)) { case (b, lb, rb) => Branch(b, lb, rb) }
      case Leaf(value) => Applikative[G].map(f(value))(Leaf(_))
    }
  }

  implicit val treePostorderDFSTraversable: Traversable[Tree] = new Traversable[Tree] {
    def traverse[G[_] : Applikative, A, B](tree: Tree[A])(f: A => G[B]): G[Tree[B]] = tree match {
      case Branch(value, left, right) =>
        Applikative[G].map3(traverse(left)(f), traverse(right)(f), f(value)) { case (lb, rb, b) => Branch(b, lb, rb) }
      case Leaf(value) => Applikative[G].map(f(value))(Leaf(_))
    }
  }

  implicit val treeBFSTraversable: Traversable[Tree] = new Traversable[Tree] {
    def traverse[G[_] : Applikative, A, B](tree: Tree[A])(f: A => G[B]): G[Tree[B]] =
      ???
  }
}

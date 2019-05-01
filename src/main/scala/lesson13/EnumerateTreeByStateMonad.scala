package lesson13

import cats.data.State
import lesson11.{Branch, Leaf, Tree}

object EnumerateTreeByStateMonad extends App {

  def enumerate: Tree[Unit] => Tree[Int] = tree => {

    def go(tree: Tree[Unit]): State[Int, Tree[Int]] =
      tree match {
        case Leaf(_) =>
          for {
            v <- State.get[Int]
            _ <- State.modify[Int](_ + 1)
          } yield Leaf(v)
        case Branch(_, left, right) =>
          for {
            v <- State.get[Int]
            _ <- State.modify[Int](_ + 1)
            l <- go(left)
            r <- go(right)
          } yield Branch(v, l, r)
      }

    go(tree).runA(0).value
  }

  //        (0)
  //       /   \
  //     (1)   (2)
  //          /   \
  //        (3)   (8)
  //       /   \
  //     (4)   (7)
  //    /   \
  //  (5)   (6)

  val tree =
    Branch((),
      Leaf(()),
      Branch((),
        Branch((),
          Branch((),
            Leaf(()),
            Leaf(())
          ),
          Leaf(())
        ),
        Leaf(())
      )
    )

  println(enumerate(tree))
}

package lesson15

object AdHocUnionsInitial extends App {

  // simple ADTs
  sealed trait Animal extends Product with Serializable
  case class Cat(color: Int) extends Animal
  case class Dog(name: String) extends Animal

  sealed trait Plant extends Product with Serializable
  case class Tree(height: Int) extends Plant
  case class Herb(taste: String) extends Plant

  val list = List(Cat(12), Dog("good boi")) ++ List(Tree(2), Herb("sweet")) // LUB = Product with Serializable

  // UNSOUND PATMATCH
  val res = list.map {
    case Cat(color) => s"Cat($color)"
    case Dog(name) => s"Dog($name)"
    case Tree(height) => s"Tree($height)"
    case Herb(taste) => s"Herb($taste)" // COMMENT THIS OUT
  }
  println(res)
}


// Props to Oleg Nizhnik for the idea
object AdHocUnionsFinal extends App {
  trait Initial[-F[_]] { def fold[R](fa: F[R]): R }

  object Initial {
    type Cons[F[_]] = F[Initial[F]]

    def apply[F[_]] = new ApplyPartially[F]

    type T // Alex Konovalov's trick, see: https://github.com/alexknvl/polymorphic
    class ApplyPartially[F[_]] {
      def apply(f: F[T] => T): Initial[F] = new Initial[F] {
        override def fold[R](fa: F[R]): R = f(fa.asInstanceOf[F[T]]).asInstanceOf[R]
      }
    }
  }

  trait Animal[+A] {
    def cat(color: Int): A
    def dog(name: String): A
  }
  object Animal extends Initial.Cons[Animal] {
    def cat(color: Int): Initial[Animal] = Initial[Animal](_.cat(color))
    def dog(name: String): Initial[Animal] = Initial[Animal](_.dog(name))
  }

  trait Plant[+A] {
    def tree(height: Int): A
    def herb(taste: String): A
  }
  object Plant extends Initial.Cons[Plant] {
    def tree(height: Int): Initial[Plant] = Initial[Plant](_.tree(height))
    def herb(taste: String): Initial[Plant] = Initial[Plant](_.herb(taste))
  }

  val listBB = List(Animal.cat(12), Animal.dog("good boi")) ++ List(Plant.tree(2), Plant.herb("sweet"))
  // LUB = Initial[λ[x => Animal[x] with Plant[x]]]

  // EXHAUSTIVE PATMATCH
  val resBB = listBB.map(_.fold(new Animal[String] with Plant[String] {
    def cat(color: Int) = s"Cat($color)"
    def dog(name: String) = s"Dog($name)"
    def tree(height: Int) = s"Tree($height)"
    def herb(taste: String) = s"Herb($taste)"
  }))

  println(resBB)

}
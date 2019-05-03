package lesson14

import lesson14.laws.MonadLaws

object MonadLawsCheck extends App with MonadLaws with UtilMonadInstances {

  println(
    "\neitherMonad:\n" +
      "leftIdentity: " + eitherMonadLeftIdentity[Int, Double, String](10.0, (n: Double) => Right(n.toString + "!")) + "\n" +
      "rightIdentity: " + eitherMonadRightIdentity[Int, Double](Right(2.0)) + "\n" +
      "mapFlatMapCoherence: " + eitherMapFlatMapCoherence[Int, Double, String](Right(110.9), (n: Double) => n.toString)
  )

  println(
    "\naAndFMonad:\n" +
      "leftIdentity: " + aAndFMonadLeftIdentity(10, (n: Int) => (n.toDouble, List(n.toDouble))) + "\n" +
      "rightIdentity: " + aAndFMonadRightIdentity((10, List(1))) + "\n" +
      "mapFlatMapCoherence: " + aAndFMapFlatMapCoherence((10, List(1)), (_: Int) * 10)
  )

  println(
    "\naOrFMonad:\n" +
      "leftIdentity: " + aOrFMonadLeftIdentity[List, Int, Double](10, (n: Int) => Right(List(n.toDouble))) + "\n" +
      "rightIdentity: " + aOrFMonadRightIdentity[List, Int](Left(4)) + "\n" +
      "mapFlatMapCoherence: " + aOrFMapFlatMapCoherence[List, Int, String](Left(2), (_: Int) + "!")
  )

}
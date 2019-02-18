package lesson6.instances

import cats.Monoid
import cats.kernel.{Band, BoundedSemilattice, CommutativeMonoid, CommutativeSemigroup, Semigroup, Semilattice}
import lesson6.BoundedBand

trait MapInstances extends MapInstances1

trait MapInstances1 extends MapInstances2 {
  implicit def mapMergeBoundedSemilattice[K, V: Semilattice] = new BoundedSemilattice[Map[K, V]]
    with MapMergeCombine[K, V] with MapEmpty[K, V] {
    val V = Semilattice[V]
  }
}

trait MapInstances2 extends MapInstances3 {
  implicit def mapMergeBoundedBand[K, V: Band] = new BoundedBand[Map[K, V]]
    with MapMergeCombine[K, V] with MapEmpty[K, V] {
    val V = Band[V]
  }
}

trait MapInstances3 extends MapInstances4 {
  implicit def mapMergeCommutativeMonoid[K, V: CommutativeSemigroup] = new CommutativeMonoid[Map[K, V]]
    with MapMergeCombine[K, V] with MapEmpty[K, V] {
    val V = CommutativeSemigroup[V]
  }
}

trait MapInstances4 extends MapInstances5 {
  implicit def mapMergeMonoid[K, V: Semigroup] = new Monoid[Map[K, V]]
    with MapMergeCombine[K, V] with MapEmpty[K, V] {
    val V = Semigroup[V]
  }

}

trait MapInstances5 {
  implicit def mapReplaceMonoid[K, V] = new Monoid[Map[K, V]]
    with MapReplaceCombine[K, V] with MapEmpty[K, V]
}

trait MapMergeCombine[K, V] {
  protected implicit def V: Semigroup[V]
  import cats.syntax.semigroup._ // |+|

  def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] = {
    val xKeys = x.keySet
    val yKeys = y.keySet
    val intersectionKeys = xKeys intersect yKeys
    val intersectionMap = intersectionKeys.map(key ⇒ (key, x(key) |+| y(key))).toMap
    (x -- intersectionKeys) ++ (y -- intersectionKeys) ++ intersectionMap
  }
}

trait MapReplaceCombine[K, V] {
  def combine(x: Map[K, V], y: Map[K, V]): Map[K, V] = x ++ y
}

trait MapEmpty[K, V] {
  def empty: Map[K, V] = Map.empty[K, V]
}

/*
  implicit def mapReplaceMonoidK[K]: MonoidK[Map[K, ?]] = new MonoidK[Map[K, ?]] {
    def empty[V]: Map[K, V] = Map.empty[K, V]

    // <+>
    def combineK[V](a: Map[K, V], b: Map[K, V]): Map[K, V] = a ++ b
  }

  Map("x" → 1) |+| Map("x" → 2) == Map("x" → 3)

  Map("x" → 1) <+> Map("x" → 2) == Map("x" → 2)
*/
package lesson6

import cats.Monoid
import cats.kernel.Band

trait BoundedBand[A] extends Monoid[A] with Band[A]

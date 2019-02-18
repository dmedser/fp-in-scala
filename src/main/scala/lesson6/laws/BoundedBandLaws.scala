package lesson6.laws

import cats.kernel.laws.{BandLaws, MonoidLaws}
import lesson6.BoundedBand

trait BoundedBandLaws[A] extends BandLaws[A] with MonoidLaws[A] {
  implicit def S: BoundedBand[A]
}

object BoundedBandLaws {
  def apply[A](implicit ev: BoundedBand[A]): BoundedBandLaws[A] =
    new BoundedBandLaws[A] { def S: BoundedBand[A] = ev }
}
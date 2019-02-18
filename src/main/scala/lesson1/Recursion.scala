package lesson1

import cats.data.Chain
import com.comcast.ip4s.IpAddress
import cats.syntax.order._

object Recursion {

  case class IpRange private (start: IpAddress, end: IpAddress) {
    def contains(ipAddress: IpAddress): Boolean = start <= ipAddress && end >= ipAddress
    def slowList: List[IpAddress] = {
      @annotation.tailrec
      def go(next: IpAddress, acc: List[IpAddress]): List[IpAddress] = next match {
        case n if n <= end => go(n.next, acc :+ n)
        case _ => acc
      }
      go(start, List())
    }

    def fastList: List[IpAddress] = {
      @annotation.tailrec
      def go(next: IpAddress, acc: Chain[IpAddress]): List[IpAddress] = next match {
        case n if n <= end => go(n.next, acc :+ n)
        case _ => acc.toList
      }
      go(start, Chain.empty[IpAddress])
    }
  }

  object IpRange {
    def apply(start: String, end: String): Option[IpRange] =
      for {
        s <- IpAddress(start)
        e <- IpAddress(end)
        if s < e
      } yield IpRange(s, e)
  }

}

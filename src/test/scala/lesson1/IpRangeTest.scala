package lesson1

import java.net.InetAddress

import com.comcast.ip4s._
import org.scalatest.{MustMatchers, WordSpec}

class IpRangeTest extends WordSpec with MustMatchers {

  import lesson1.Recursion._
  import IpRangeTest._

  "IpRangeTest" must {
    "work for correct ip range" in {
      val start = "10.0.0.1"
      val end = "10.0.0.5"

      val range = IpRange(start, end).get

      range.contains(ip"10.0.0.1") mustBe true
      range.contains(ip"10.0.0.3") mustBe true
      range.contains(ip"10.0.0.5") mustBe true

      range.contains(ip"10.0.0.0") mustBe false
      range.contains(ip"10.0.0.6") mustBe false

      range.contains(ip"127.0.0.1") mustBe false
    }

    "fail on wrong range" in {
      IpRange("10.0.0.5", "10.0.0.1") mustBe None
    }

    "build list from range" in {
      val range1 = IpRange("10.0.0.1", "10.0.0.2").get
      range1.fastList mustBe List(ip"10.0.0.1", ip"10.0.0.2")

      val range2 = IpRange("10.0.0.1", "10.0.0.3").get
      range2.fastList mustBe List(ip"10.0.0.1", ip"10.0.0.2", ip"10.0.0.3")
    }

    "correctly check java ip and custom ip equality" in {
      val javaIp = InetAddress.getByName("127.0.0.1")
      val customIp = ip"127.0.0.1"
      javaIp mustBe customIp.toInetAddress
    }


    "must calculate range fast" in {
      val (_, res1) = measure { IpRange("10.243.224.0", "10.243.254.0").get.fastList }
      val (_, res2) = measure { IpRange("10.243.252.0", "10.243.252.44").get.fastList }
      val (_, res3) = measure { IpRange("10.243.167.0", "10.243.254.0").get.fastList }
      val (_, res4) = measure { IpRange("10.243.252.0","10.243.252.46").get.fastList }

      (res1 + res2 + res3 + res4) must be <= 100L
    }

  }

}

object IpRangeTest {
  def measure[T](block: => T): (T, Long) = {
    val t0 = System.currentTimeMillis
    val res = block
    val end = System.currentTimeMillis - t0
    (res, end)
  }
}

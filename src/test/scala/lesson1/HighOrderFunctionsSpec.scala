package lesson1

import org.scalatest.{MustMatchers, WordSpec}

class HighOrderFunctionsSpec extends WordSpec with MustMatchers {

  import lesson1.HighOrderFuncs._

  "HighOrderFunctionsSpec" must {

    "promote salaries correctly" in {
      val sls = List(10, 100, 1000)
      SalaryRaiser.smallPromotion(sls) mustBe List(10 * 10, 100 * 10, 1000 * 10)
      SalaryRaiser.greatPromotion(sls) mustBe List(10 * 100500, 100 * 100500, 1000 * 100500)
      SalaryRaiser.hugePromotion(sls) mustBe List(10 * 10, 100 * 100, 1000 * 1000)
    }

    "build url correctly" in {
      val domainName = "rbo.raiffeisen.ru"
      def getUrl = UrlBuilder.urlBuilder(ssl = true, domainName)

      val endPoint = "accounts"
      val query = "org_name=Leroy Merlin"

      getUrl(endPoint, query) mustBe "https://rbo.raiffeisen.ru/accounts/org_name=Leroy Merlin"
    }

  }

}

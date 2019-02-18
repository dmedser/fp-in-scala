package lesson1

object HighOrderFuncs {

  object SalaryRaiser {

    private def promotion(salaries: List[Int], promotionFunction: Int => Int) =
      salaries.map(promotionFunction)

    def smallPromotion(salaries: List[Int]): List[Int] = promotion(salaries, salary => salary * 10)

    def greatPromotion(salaries: List[Int]): List[Int] = promotion(salaries, salary => salary * 100500)

    def hugePromotion(salaries: List[Int]): List[Int] = promotion(salaries, salary => salary * salary)
  }

  object UrlBuilder {
    def urlBuilder(ssl: Boolean, domainName: String): (String, String) => String = {
      val schema = if (ssl) "https://" else "http://"
      (endPoint: String, query: String) =>
        s"$schema$domainName/$endPoint/$query"
    }
  }

}

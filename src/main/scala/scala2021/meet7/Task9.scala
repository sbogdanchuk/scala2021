package scala2021.meet7

//ayafimau
object Task9 extends App {

  val result = 42(USD) + 35(EUR)

  val resultToPound = result to GBP
  println(resultToPound)

  val resultToEUR = result to EUR
  println(resultToEUR)
  val resultToUSD = result to USD
  println(resultToUSD)
  println(result)

  implicit class MoneyOps[T](x: T)(implicit n: Numeric[T]) {

    def apply(currency: Currency): Money = {
      // NOTE: toString is surprisingly a recommended method for converting Double to BigDecimal (due to rounding / precision)
      // NOTE:  we allow even negative values for money, since it might be applicable in financial mathematics
      Money(BigDecimal(x.toString), currency)
    }
  }

  case class Money(sum: BigDecimal, currency: Currency) {

    // operator "+" defaults to USD conversion (coercion) as the base currency,  may be discussed as per requirements
    def +(that: Money): Money = Money(to(USD) + that.to(USD), USD)

    def to(currencyTo: Currency): Double = {
      if (currency == currencyTo) // just not to lose precision on conversion back & forth
        this.sum.setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
      else {
        val sumInUSD = sum * ratesToUSD(currency)
        val sumInTarget = sumInUSD / ratesToUSD(currencyTo)
        sumInTarget.setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
      }
    }

    // denotes conversion rates of 1 "currency unit" to USD
    val ratesToUSD:Map[Currency, Double] = Map(USD -> 1.0, EUR -> 1.2, GBP -> 1.39)
  }


  sealed trait Currency // to be used in Maps

  case object USD extends Currency

  case object EUR extends Currency

  case object GBP extends Currency

}

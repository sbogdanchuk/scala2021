package scala2021.ashinkarev.task01

object Program1 extends App {
  val counts = Array(
    "900,google.com",
    "60,mail.yahoo.com",
    "10,mobile.sports.yahoo.com",
    "40,sports.yahoo.com",
    "10,stackoverflow.com",
    "2,en.wikipedia.org",
    "1,es.wikipedia.org",
    "1,mobile.sports"
  )

  counts
  // args
    .map(e => e.split(","))
    .map(toSubdomainsWithNumber)
    .flatten
    .groupBy(e => e._1)
    .map(group => (group._1, group._2.map(e => e._2).sum))
    .toSeq
    .sortWith(_._2 > _._2)
    .foreach(e => println(s"${e._1} ${e._2}"))

  def toSubdomainsWithNumber(numberAndDomain: Array[String]) : Array[(String, Int)] = {
    val number = numberAndDomain(0).toInt
    val domain = numberAndDomain(1)
    
    var result = Array((domain, number))

    for (i <- 0 to domain.length() - 1) {
      val currentSymbol = domain.charAt(i)

      if (currentSymbol == '.') {
        result :+= (domain.substring(i + 1), number)
      }
    }

    return result
  }
}

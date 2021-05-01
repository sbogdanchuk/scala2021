package scala2021.meet3

//ayafimau
object MainImmutable extends App {
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

  val pattern = "(\\d+),(.*)".r

  val rolledUpCounts = counts.flatMap(record => {
    val pattern(hitCount, domain) = record

    val extractDomainsPattern = "(?=(?:^|\\.)(.*))".r
    extractDomainsPattern.findAllIn(domain).matchData
      .map(m => (hitCount.toInt, m.group(1)))
  })
    .groupBy(_._2)
    .map { case (domain, domainHitsArray) =>
      domain -> domainHitsArray.map {_._1}.sum
    }

  println(rolledUpCounts.map { case (k, v) => s"$v $k\r\n" }.mkString)
}

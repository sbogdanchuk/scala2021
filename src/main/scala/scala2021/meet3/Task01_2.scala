package scala2021.meet3

//ypozdnyakov
object Task1 {
  def main(args: Array[String]): Unit = {
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

    getTotalsByDomain(counts)
  }

  def getTotalsByDomain(domainList: Array[String]) = {
    domainList
      .flatMap { str =>
        val Array(num, fullDomain) = str.split(",")
        fullDomain
          .split("\\.")
          .tails
          .collect {
            case domain if domain.nonEmpty => (domain.mkString("."), num.toInt)
          }
      }
      .groupMapReduce(_._1)(_._2)(_ + _)
      .toList
      .foreach {
        case (value, index) => {
          println(s"$value $index")
        }
      }
  }
}

package scala2021.meet4

import scala.annotation.tailrec

//dkharlap
object Task4_2 {
  def main(args: Array[String]): Unit = {
    val coins = List[Int](2, 5, 6)
    println("Change is possible=" + checkChangePossibility(13, coins))
  }

  def checkChangePossibility(money: Int, coins: List[Int]): Boolean = {
    @tailrec
    def canChange(possibleSums: List[Int]): Boolean = {
      val targetSums = possibleSums.distinct.filter(sum => sum <= money)
      if (targetSums.isEmpty) false
      else if (targetSums.contains(money)) true
      else {
        val generatedSums = targetSums.flatMap(sum => coins.map(coin => sum + coin))
        canChange(generatedSums)
      }
    }

    canChange(coins)
  }
}

package scala2021.meet4

import scala.annotation.tailrec

//ayafimau
object Task4_1 extends App {

  val coins = List(2, 4, 6)

  val k = 8

  println(canChange(k))

  def canChange(sum: Int): Boolean = {

    @tailrec def canChangeInternal(possibleSums: List[Int]): Boolean = {
      val targetSums = possibleSums.distinct.filter(x => x <= sum)
      if (targetSums.isEmpty)
        false
      else if (targetSums.contains(sum))
        true
      else {
        val spawnedSums = targetSums.flatMap(x =>
          coins.map(coin => x + coin)
        )
        canChangeInternal(spawnedSums)
      }
    }

    canChangeInternal(coins)
  }
}

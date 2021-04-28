package scala2021.meet3

import scala.annotation.tailrec

object Task03 extends App {
  val source = "aaaabccaadeeee".toList.map(_.toString).map(Symbol(_))
  val expected = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))

  def encodeDirect(list: List[Symbol]): List[(Int, Symbol)] = {
    @tailrec
    def loop(acc: List[(Int, Symbol)], list: List[Symbol]): List[(Int, Symbol)] = list match {
      case Nil => acc
      case head :: _ =>
        val (taken, rest) = list.span(_ == head)
        loop((taken.length, head) +: acc, rest)
    }

    loop(Nil, list).reverse
  }

  println(expected)
  println(encodeDirect(source))
}

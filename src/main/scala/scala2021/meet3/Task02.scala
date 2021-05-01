package scala2021.meet3

import scala.annotation.tailrec

object Task02 extends App {
  def balance(str: String): Boolean = {
    @tailrec
    def loop(chars: List[Char], b: Int): Boolean = chars match {
      case _ if b < 0 => false
      case Nil => b == 0
      case '(' :: tail => loop(tail, b + 1)
      case ')' :: tail => loop(tail, b - 1)
      case _ :: tail => loop(tail, b)
    }

    loop(str.toList, 0)
  }

  val testStrings = Array("if((2+x)*(3-y)==3)",
    "Я сказал ему (это еще (не) сделано). (Но он не послушал)",
    ":-)",
    "())("
  )

  testStrings.map(e => (e, balance(e))).foreach {
    case (str, isBalanced) => println(s"$isBalanced, $str")
  }
}

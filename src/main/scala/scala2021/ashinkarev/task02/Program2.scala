package scala2021.ashinkarev.task02

import scala.annotation.tailrec

object Program2 extends App {
  // val cases = Array(
  //   "if((2+x)*(3-y)==3)",
  //   "Я сказал ему (это еще (не) сделано). (Но он не послушал)",
  //   ":-)",
  //   "())("
  // )

  // cases
  args
    .foreach(e => println(s"${e} ${checkRoundBrackets(e.toList)}"))

  def checkRoundBrackets(symbols: List[Char]): Boolean = {
    @tailrec
    def checkRoundBrackets(symbols: List[Char], accumulator: Int): Boolean = {
      if (accumulator < 0) {
        return false
      }

      if (symbols.length == 0) {
        return accumulator == 0
      }

      symbols(0) match {
        case '(' => checkRoundBrackets(symbols.slice(1, symbols.length), accumulator + 1)
        case ')' => checkRoundBrackets(symbols.slice(1, symbols.length), accumulator - 1)
        case default => checkRoundBrackets(symbols.slice(1, symbols.length), accumulator)
      }
    }
    checkRoundBrackets(symbols, 0)
  }
}

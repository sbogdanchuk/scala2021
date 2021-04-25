package scala2021.ashinkarev.task03

object Program3 extends App {
  val listOrSymbols = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  // val listOrSymbols = List('a', 'b', 'c')
  // val listOrSymbols = List('a', 'a', 'b')
  // val listOrSymbols = List('a', 'a')
  // val listOrSymbols = List('a')

  encodeDirect(listOrSymbols)
    .foreach(println)

  def encodeDirect(symbols: List[Char]): List[(Int, Char)] = {
    if (symbols.length == 0) {
      return List.empty[(Int, Char)]
    }

    var result = Array[(Int, Char)]()

    var prevSymbol = symbols(0)
    var count = 1

    val symbolsExceptForTheFirstOne = symbols.slice(1, symbols.length)

    if (symbolsExceptForTheFirstOne.length == 0) {
      result :+= (count, prevSymbol)
    }

    for (i <- 0 to symbolsExceptForTheFirstOne.length - 1) {
      val symbol = symbolsExceptForTheFirstOne(i)

      if (symbol == prevSymbol) {
        count += 1
      } else {
        result :+= (count, prevSymbol)
        
        prevSymbol = symbol
        count = 1
      }

      if (i == symbolsExceptForTheFirstOne.length - 1) {
        result :+= (count, prevSymbol)
      }
    }

    return result.toList
  }
}

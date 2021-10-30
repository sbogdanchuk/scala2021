package scala2021.meet7

//vmylnikov
object Main extends App {

  val Digit = "([0-9])".r

  def parseLine(str: String): List[Int] = {
    parseFrames(str.replace("|", "").toList.map(_.toString))
  }

  def parseFrames(chars: List[String]): List[Int] = chars match {
    case Nil => Nil
    case "X" :: rest => 10 :: parseFrames(rest)
    case "-" :: rest => 0 :: parseFrames(rest)
    case "/" :: rest => 10 :: parseFrames(rest)
    case Digit(c) :: "/" :: rest => c.toInt :: (10 - c.toInt) :: parseFrames(rest)
    case Digit(c) :: rest => c.toInt :: parseFrames(rest)
  }

  def scoreLine(line: List[Int]): Int = line match {
    case Nil => 0
    case a :: b :: c :: Nil if isStrike(a) => a + b + c
    case a :: b :: c :: Nil if isSpare(a, b) => a + b + c
    case a :: b :: c :: rest if isStrike(a) => 10 + b + c + scoreLine(b :: c :: rest)
    case a :: b :: c :: rest if isSpare(a, b) => 10 + c + scoreLine(c :: rest)
    case a :: b :: rest => a + b + scoreLine(rest)
  }

  def isStrike(a: Int): Boolean = a == 10

  def isSpare(a: Int, b: Int): Boolean = a + b == 10

  def score(line: String): Int = {
    scoreLine(parseLine(line))
  }

  assert(score("X|X|X|X|X|X|X|X|X|X||XX") == 300)
  assert(score("9-|9-|9-|9-|9-|9-|9-|9-|9-|9-||") == 90)
  assert(score("5/|5/|5/|5/|5/|5/|5/|5/|5/|5/||5") == 150)
  assert(score("X|7/|9-|X|-8|8/|-6|X|X|X||81") == 167)
  assert(score("5/|4-|81|X|-/|X|X|X|4/|X||X5") == 186)
  assert(score("5/|4-|81|X|--|X|X|X|4/|X||X5") == 156)

}

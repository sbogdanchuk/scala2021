package scala2021.sbahdanchuk.meet

import scala.util.{Failure, Success, Try}

object Meet2 extends App {

  println("\n<<<<<<<Strings>>>>>>>")
  val s = "The quick brown fox jumps over the lazy doog"
  println(s)

  def stringOps(s: String): String = {
    def go(chars: List[Char]): List[Char] = chars match {
      case ch :: tail if ch.isUpper => ch.toLower :: go(tail)
      case 'o' :: 'x' :: tail => 'x' :: 'o' :: go(tail)
      case a :: b :: tail if a == b => a :: go(tail)
      case ch :: tail if ch.isLower => ch.toUpper :: go(tail)
      case ch :: tail => ch :: go(tail)
      case Nil => Nil
    }

    go(s.toList).mkString
  }

  println(stringOps(s))

  println("\n<<<<<<<Monads>>>>>>>")
  println("Monad.Option")

  val list = List(0, null, 2, 3, 4, 5, " ") map {
    case x: Int => Option(x)
    case _ => None
  }
  println(list)
  println(list filter (_.isDefined))

  println("Monad.Either")

  val option2either = List(Some("qwerty"), None) map {
    case Some(x) => Right(x)
    case None => Left("Please do not send us nulls.")
  }
  println(option2either)

  val eitherList = List(0, 1, 2, 3, 4, 5, " ", ' ') map {
    case x: Int => Right(x)
    case s: String => Left("no string please")
    case _ => Left("??")
  }

  println(eitherList)
  val (right, left) = eitherList.partition(_.isRight)
  println(s"Left only: ${left}")
  println(s"Right only: ${right}")
  println(right map (_.getOrElse(-1)))

  println("\nMonad.Try")

  def divideByN(n: Int): Int = 1 / n

  private val tryList = List(-1, 0, 1)
  println(tryList)
  val divided = tryList
    .map(e => Try(divideByN(e)))

  println(divided)

  val handleFails = (Failure(new IndexOutOfBoundsException("just another exception example")) :: divided).map { //add one more exception
    case Success(v) => v
    case Failure(err) => err match {
      case _: ArithmeticException => Double.MaxValue
      case ex: IndexOutOfBoundsException => -33
    }
  }

  println(s"handleFails: $handleFails")

  println("\n<<<<<<<For-comprehension>>>>>>>")
  ////  for
  ////    for
  //
  val forfor = for {
    i <- 0 to 2
    j <- 0 to 2
    k = i + j
    if k % 2 == 0
  } yield (i, j)

  println(forfor)

  val forforDesugared = (0 to 2)
    .flatMap(i =>
      (0 to 2)
        .map { j => val k = i + j; (j, k) }
        .withFilter { case (j, k) => k % 2 == 0 }
        .map { case (j, k) => (i, j) }
    )

  println(forforDesugared)

  println("\n<<<<<<<Streams aka LazyLists>>>>>>>")
  val fibs: LazyList[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map {
    case (a, b) => a + b
  }
  println(fibs.take(11).toList)

  val ints: LazyList[Int] = {
    def loop(v: Int): LazyList[Int] = v #:: loop(v + 1)

    loop(0)
  }

  println(s"posInts: ${ints.take(11).toList}")

  val even = ints.map(_ * 2)
  println(s"even: ${even.take(11).toList}")


}

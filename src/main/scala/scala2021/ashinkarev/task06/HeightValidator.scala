package scala2021.ashinkarev.task06

import scala2021.ashinkarev.task06.Sex._

object HeightValidator {
  def isHeightValid(sex: Sex, height: Double): Either[String, Boolean] = {
    sex match {
      case Female => Right(true)
      case Male if height > 100 => Right(true)
      case Male if height <= 100 => Left("Male height should be > 100.")
    }
  }
}

package scala2021.ashinkarev.task06

object AgeValidator {
  def isAgeValid(age: Int): Either[String, Boolean] = {
    return if (age > 0 && age < 100) Right(true) else Left("Only positive age that is less than 100 allowed.");
  }
}

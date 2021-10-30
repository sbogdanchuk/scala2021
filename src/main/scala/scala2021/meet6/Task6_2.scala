package scala2021.meet6

import cats.data.Validated._
import cats.data._
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

//sbahdanchuk
object ValidatoriumNec {

  sealed trait FormValidatorNec {
    val validEmailRegex = "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])"
    type ValidationResult[A] = ValidatedNec[DomainValidation, A]

    private def validateUserName(userName: String): ValidationResult[String] =
      if (userName.matches("^[a-zA-Z]+$")) userName.validNec else UserNameHasNonLatinCharacters.invalidNec

    private def validateAge(age: Int): ValidationResult[Int] =
      if (age > 0 && age < 100) age.validNec else UserAgeIsOutOfRange.invalidNec

    private def validateEmail(email: String): ValidationResult[String] =
      if (email !=null && (email.isEmpty || email.matches(validEmailRegex)))
        email.validNec else UserEmailIsInvalid.invalidNec

    private def validateHeight(sex: Sex, height: Double): ValidationResult[Double] =
      if ((sex == Sex.Male && height > 100) || (sex == Sex.Female && height > 0)) height.validNec else UserHeightIsTooLow.invalidNec

    def validateForm(name: String, age: Int, email: String, sex: Sex, height: Double): ValidationResult[User] = {
      (validateUserName(name),
        validateAge(age),
        validateEmail(email),
        sex.validNec,
        validateHeight(sex, height)).mapN(User)
    }

    def validateFormPar(name: String, age: Int, email: String, sex: Sex, height: Double): Future[ValidationResult[User]] = {

      val nameF = Future(validateUserName(name))
      val ageF = Future(validateAge(age))
      val emailF = Future(validateEmail(email))
      val sexF = Future(sex.validNec)
      val heightF = Future(validateHeight(sex, height))
      for {
        name <- nameF
        age <- ageF
        email <- emailF
        sex <- sexF
        height <- heightF
      } yield (name, age, email, sex, height).mapN(User)
    }
  }

  object FormValidatorNec extends FormValidatorNec
}

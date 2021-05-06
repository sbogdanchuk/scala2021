package scala2021.ashinkarev.task06

import scala2021.ashinkarev.task06.Sex._

case class Form(
  name: String,
  age: Int, 
  email: String, 
  sex: Sex,
  height: Double
)

object FormValidator {
  def isFormValid(form: Form): Either[String, Boolean] = {
    for {
      isNameValid <- NameValidator.isNameValid(form.name)
      isAgeValid <- AgeValidator.isAgeValid(form.age)
      isEmailValid <- EmailValidator.isEmailValid(form.email)
      isHeightValid <- HeightValidator.isHeightValid(form.sex, form.height)
    } yield isHeightValid
  }
}

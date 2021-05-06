package scala2021.ashinkarev.task06

import scala2021.ashinkarev.task06.Sex._

case class Form(
  name: String,
  age: Int, 
  email: String, 
  sex: Sex,
  height: Double
)

object Program extends App {
  def isValidName(name: String): Either[String, Boolean] = {
    return NameValidator.isNameValid(name);
  }
}

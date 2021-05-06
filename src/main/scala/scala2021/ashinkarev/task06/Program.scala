package scala2021.ashinkarev.task06

case class Form(name: String, age: Int, email: String)

object Program extends App {
  def isValidName(name: String): Either[String, Boolean] = {
    return NameValidator.isNameValid(name);
  }
}

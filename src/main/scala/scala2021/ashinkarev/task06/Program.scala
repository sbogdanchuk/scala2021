package scala2021.ashinkarev.task06

case class Form(name: String, age: Int, email: String)

object Program extends App {
  // Найти имя менеджера департамента, в котором работает сотрудник по имени сотрудника
  def isValidName(name: String): Either[String, Boolean] = {
    return NameValidator.isValidName(name);
  }
}

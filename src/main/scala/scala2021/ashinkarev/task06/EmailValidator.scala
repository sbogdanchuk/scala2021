package scala2021.ashinkarev.task06

object EmailValidator {
  // https://html.spec.whatwg.org/multipage/input.html#valid-e-mail-address
  private val emailRegex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

  def isEmailValid(email: String): Either[String, Boolean] = {
    email match {
      case email if email.isEmpty() => Right(true)
      case email if emailRegex.findFirstMatchIn(email).isDefined => Right(true)
      case _ => Left("Email is invalid.")
    }
  }
}

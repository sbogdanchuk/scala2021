package scala2021.meet6

import enumeratum.{Enum, EnumEntry}

sealed trait Sex extends EnumEntry

object Sex extends Enum[Sex] {
  val values = findValues

  case object Male extends Sex

  case object Female extends Sex
}

case class User(name: String, age: Int, email: String, sex: Sex, height: Double)

sealed trait DomainValidation {
  def errorMessage: String
}

case object UserNameHasNonLatinCharacters extends DomainValidation {
  override def errorMessage: String = "Username cannot contain nonlatin characters."
}

case object UserAgeIsOutOfRange extends DomainValidation {
  override def errorMessage: String = "User age should be in range (0..100)"
}

case object UserEmailIsInvalid extends DomainValidation {
  override def errorMessage: String = "User email should be valid"
}

case object UserHeightIsTooLow extends DomainValidation {
  override def errorMessage: String = "User height is too low"
}
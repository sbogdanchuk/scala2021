package scala2021.ashinkarev.task06

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class EmailValidatorSuite extends AnyFunSuite with TableDrivenPropertyChecks with ScalaCheckDrivenPropertyChecks with Matchers {
  val expectedError = Left("Email is invalid.");

  import EmailValidator.isEmailValid

  test("isEmailValid for empty string => true") {
    isEmailValid("") should be (Right(true))
  }

  test("isEmailValid for correct emails => true") {
    isEmailValid("a@b.c") should be (Right(true))
    isEmailValid("my-email@example.com") should be (Right(true))
  }

  test("isEmailValid for incorrect emails => returns error") {
    isEmailValid("@example.com") should be (expectedError)
    isEmailValid("@") should be (expectedError)
    isEmailValid("@.") should be (expectedError)
    isEmailValid("Good day!") should be (expectedError)
    isEmailValid("text") should be (expectedError)
  }
}

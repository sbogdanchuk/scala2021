package scala2021.ashinkarev.task06

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class NameValidatorSuite extends AnyFunSuite with TableDrivenPropertyChecks with ScalaCheckDrivenPropertyChecks with Matchers {

  import NameValidator.isNameValid

  test("isValidName for empty string => true") {
    isNameValid("") should be (Right(true))
  }

  test("isValidName for non-latin letters => returns incorrect symbols") {
    isNameValid("цыц") should be (Left(s"Only latin letters are allowed. Those are not allowed symbols: ц ы."))
    isNameValid("123") should be (Left(s"Only latin letters are allowed. Those are not allowed symbols: 1 2 3."))
    isNameValid("abcцыцops123zz") should be (Left(s"Only latin letters are allowed. Those are not allowed symbols: ц ы 1 2 3."))
    isNameValid("abc  a") should be (Left(s"Only latin letters are allowed. Those are not allowed symbols: whitespace."))
    isNameValid(", ^$") should be (Left(s"Only latin letters are allowed. Those are not allowed symbols: , whitespace ^ $$."))
  }

  test("isValidName for latin letters of any case => true") {
    isNameValid("Zzzabc") should be (Right(true))
  }
}

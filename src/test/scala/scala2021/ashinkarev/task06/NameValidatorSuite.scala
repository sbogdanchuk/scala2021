package scala2021.ashinkarev.task06

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class NameValidatorSuite extends AnyFunSuite with TableDrivenPropertyChecks with ScalaCheckDrivenPropertyChecks with Matchers {

  import NameValidator.isValidName

  test("isValidName for empty string => true") {
    isValidName("") should be (Right(true))
  }

  test("isValidName for non-latin letters => returns incorrect symbols") {
    isValidName("цыц") should be (Left(s"Only latin letters are allowed. Those are not allowed symbols: ц ы."))
    isValidName("123") should be (Left(s"Only latin letters are allowed. Those are not allowed symbols: 1 2 3."))
    isValidName("abcцыцops123zz") should be (Left(s"Only latin letters are allowed. Those are not allowed symbols: ц ы 1 2 3."))
    isValidName("abc  a") should be (Left(s"Only latin letters are allowed. Those are not allowed symbols: whitespace."))
    isValidName(", ^$") should be (Left(s"Only latin letters are allowed. Those are not allowed symbols: , whitespace ^ $$."))
  }

  test("isValidName for latin letters of any case => true") {
    isValidName("Zzzabc") should be (Right(true))
  }
}

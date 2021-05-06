package scala2021.ashinkarev.task06

import scala2021.ashinkarev.task06.Sex._

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class FormValidatorSuite extends AnyFunSuite with TableDrivenPropertyChecks with ScalaCheckDrivenPropertyChecks with Matchers {
  import FormValidator.isFormValid

  test("isFormValid for incorrect name => name error") {
    isFormValid(Form("Sam Smith", 99, "a@b.c", Female, 170)) should be (Left(s"Only latin letters are allowed. Those are not allowed symbols: whitespace."))
  }

  test("isFormValid for incorrect age => age error") {
    isFormValid(Form("Sam", 150, "a@b.c", Female, 170)) should be (Left("Only positive age that is less than 100 allowed."))
  }
  
  test("isFormValid for incorrect email => email error") {
    isFormValid(Form("Sam", 99, "b.c", Female, 170)) should be (Left("Email is invalid."))
  }

  test("isFormValid for incorrect height => height error") {
    isFormValid(Form("Sam", 99, "a@b.c", Male, 99)) should be (Left("Male height should be > 100."))
  }

  test("isFormValid all are incorrect => name error") {
    isFormValid(Form("Sam Smith", 150, "b.c", Male, 99)) should be (Left(s"Only latin letters are allowed. Those are not allowed symbols: whitespace."))
  }

  test("isFormValid all are valid => true") {
    isFormValid(Form("Sam", 99, "a@b.c", Female, 170)) should be (Right(true))
  }
}

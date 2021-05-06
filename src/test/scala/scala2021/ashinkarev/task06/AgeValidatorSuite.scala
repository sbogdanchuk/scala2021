package scala2021.ashinkarev.task06

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class AgeValidatorSuite extends AnyFunSuite with TableDrivenPropertyChecks with ScalaCheckDrivenPropertyChecks with Matchers {
  val expectedError = Left("Only positive age that is less than 100 allowed.");

  import AgeValidator.isAgeValid

  test("isAgeValid for 0 => returns error") {
    isAgeValid(0) should be (expectedError)
  }

  test("isAgeValid for negative number => returns error") {
    isAgeValid(-1) should be (expectedError)
    isAgeValid(-500) should be (expectedError)
  }

  test("isAgeValid for >= 100 => returns error") {
    isAgeValid(100) should be (expectedError)
    isAgeValid(101) should be (expectedError)
    isAgeValid(500) should be (expectedError)
  }

  test("isAgeValid for allowed age => true") {
    isAgeValid(1) should be (Right(true))
    isAgeValid(10) should be (Right(true))
    isAgeValid(99) should be (Right(true))
  }
}

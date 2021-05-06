package scala2021.ashinkarev.task06

import scala2021.ashinkarev.task06.Sex._

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class HeightValidatorSuite extends AnyFunSuite with TableDrivenPropertyChecks with ScalaCheckDrivenPropertyChecks with Matchers {
  val expectedError = Left("Male height should be > 100.");
  
  import HeightValidator.isHeightValid

  test("isHeightValid for Female with any height => true") {
    isHeightValid(Female, -1) should be (Right(true))
    isHeightValid(Female, 0) should be (Right(true))
    isHeightValid(Female, 1) should be (Right(true))
    isHeightValid(Female, 99) should be (Right(true))
    isHeightValid(Female, 150) should be (Right(true))
  }

  test("isHeightValid for Male with height <= 100 => returns error") {
    isHeightValid(Male, -1) should be (expectedError)
    isHeightValid(Male, 0) should be (expectedError)
    isHeightValid(Male, 1) should be (expectedError)
    isHeightValid(Male, 99) should be (expectedError)
    isHeightValid(Male, 100) should be (expectedError)
  }

  test("isHeightValid for Male with height > 100 => true") {
    isHeightValid(Male, 100.01) should be (Right(true))
    isHeightValid(Male, 150) should be (Right(true))
    isHeightValid(Male, 200) should be (Right(true))
  }
}

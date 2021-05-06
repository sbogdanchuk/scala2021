package scala2021.ashinkarev.task06

import scala.concurrent._
import ExecutionContext.Implicits.global

import scala2021.ashinkarev.task06.Sex._

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class FormValidatorSuite extends AnyFunSuite with TableDrivenPropertyChecks with ScalaCheckDrivenPropertyChecks with Matchers {
  import FormValidator.isFormValidOrError
  import FormValidator.isFormValidOrAllErrors
  import FormValidator.isFormValidParallelOrAllErrorsAsync

  val nameErrorMessage = "Only latin letters are allowed. Those are not allowed symbols: whitespace.";
  val ageErrorMessage = "Only positive age that is less than 100 allowed.";
  val emailErrorMessage = "Email is invalid.";
  val heightErrorMessage = "Male height should be > 100.";

  val singleErrorTestCasesTable = Table(
    ("form", "errorMessage"),
    (Form("Sam Smith", 99, "a@b.c", Female, 170), nameErrorMessage),
    (Form("Sam", 150, "a@b.c", Female, 170), ageErrorMessage),
    (Form("Sam", 99, "b.c", Female, 170), emailErrorMessage),
    (Form("Sam", 99, "a@b.c", Male, 99), heightErrorMessage),
  )

  test("check single field error cases") {
    forAll(singleErrorTestCasesTable) {
      (form, errorMessage) => {
        isFormValidOrError(form) should be (Left(errorMessage))
        isFormValidOrAllErrors(form) should be (Left(List(errorMessage)))
        isFormValidParallelOrAllErrorsAsync(form) map {
          parallelValidationErrors => parallelValidationErrors should be (Left(List(errorMessage)))
        }
      }
    }
  }

  test("all fields are incorrect => name error") {
    val form = Form("Sam Smith", 150, "b.c", Male, 99);

    isFormValidOrError(form) should be (Left(nameErrorMessage))
    isFormValidOrAllErrors(form) should be (Left(List(
      nameErrorMessage,
      ageErrorMessage,
      emailErrorMessage,
      heightErrorMessage
    )))

    isFormValidParallelOrAllErrorsAsync(form) map {
      parallelValidationErrors => parallelValidationErrors match {
        case Left(errors) => {
          errors should contain (nameErrorMessage);
          errors should contain (ageErrorMessage);
          errors should contain (emailErrorMessage);
          errors should contain (heightErrorMessage);
        } 
        case _ => fail("Cannot be Right")
      }
    }
  }

  test("all fields are valid => true") {
    val form = Form("Sam", 99, "a@b.c", Female, 170);

    isFormValidOrError(form) should be (Right(true))
    isFormValidOrAllErrors(form) should be (Right(true))
    isFormValidParallelOrAllErrorsAsync(form) map {
      parallelValidationErrors => parallelValidationErrors should be (Right(true))
    }
  }
}

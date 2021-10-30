package scala2021.meet6

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{convertToAnyShouldWrapper, equal}
import scala2021.meet6.Validate.{ageValidation, emailValidation, nameValidation, sexValidation}

import scala.concurrent.ExecutionContext

class ValidationSuite extends AnyFlatSpec {
  private val TaskId = "Task 06 (Validation)"

  private val validator = Validate(List(
    nameValidation,
    ageValidation,
    emailValidation,
    sexValidation
  ))

  it should s"$TaskId return Contract after successful validation" in {
    val input = Contract(Name("Success"), Age(11), Email("test@gmail.com"), SexType.Male, Height(101))
    validator.validateFirstFail(input) should equal(Right(input))
  }

  it should s"$TaskId return any first error" in {
    val input = Contract(Name("Success"), Age(11), Email("test@gmail.com"), SexType.Male, Height(100))
    validator.validateFirstFail(input) should equal(Left("Male's height should be greater than 100"))
  }

  it should s"$TaskId return all errors" in {
    val input = Contract(Name("Success11"), Age(-11), Email("testgmail.com"), SexType.Male, Height(100))
    validator.validateCollectAll(input) should equal(
      Left(List(
        "Name should have only Latin characters",
        "Age cannot be negative",
        "Email is not valid",
        "Male's height should be greater than 100")
      )
    )
  }

  it should s"$TaskId return all errors for async mode" in {
    implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global
    val input = Contract(Name("Success11"), Age(-11), Email("testgmail.com"), SexType.Male, Height(100))
    validator.validateAsync(input) map {
      result =>
        assert {
          result match {
            case Left(errors) => errors.sorted == List(
              "Name should have only Latin characters",
              "Age cannot be negative",
              "Email is not valid",
              "Male's height should be greater than 100").sorted
            case Right(_) => false
          }
        }
    }
  }

}
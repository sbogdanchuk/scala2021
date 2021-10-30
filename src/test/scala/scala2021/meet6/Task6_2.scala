package scala2021.meet6

import cats.data.Chain
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import scala2021.meet6.ValidatoriumNec.FormValidatorNec

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Success

class NecSuite extends AnyFunSuite with TableDrivenPropertyChecks with Matchers {

  val validUsers = Table(
    "user",
    User("JohnDoe", 42, "tdump@gmail.com", Sex.Male, 190)
  )
  test("check validate for valid users") {
    forAll(validUsers) {
      (user) => {
        FormValidatorNec.validateForm(user.name, user.age, user.email, user.sex, user.height) should be(Valid(user))
      }
    }
  }

  test("check parallel validate for valid users") {
    forAll(validUsers) {
      (user) => {
        val actual = FormValidatorNec.validateFormPar(user.name, user.age, user.email, user.sex, user.height)
        Await.ready(actual, Duration.Inf).value.get should be(Success(Valid(user)))
      }
    }
  }

  val invalidUsers = Table(
    ("user", "expected"),
    (User("Mord Fustang", 404, "think$this^is&Valid?@com.com", Sex.Male, 42),
      Invalid(Chain(UserNameHasNonLatinCharacters, UserAgeIsOutOfRange, UserEmailIsInvalid, UserHeightIsTooLow))),
    (User("GoodName", 404, "valid@emai.l", Sex.Female, -1),
      Invalid(Chain(UserAgeIsOutOfRange, UserHeightIsTooLow)))
  )

  test("check invalid users") {
    forAll(invalidUsers) {
      (user, expected) => {
        FormValidatorNec.validateForm(user.name, user.age, user.email, user.sex, user.height) should be(expected)
      }
    }
  }

  test("check parallel invalid users"){
    forAll(invalidUsers){
      (user, expected)=>{
        val actual=FormValidatorNec.validateFormPar(user.name, user.age, user.email, user.sex, user.height)
        Await.ready(actual, Duration.Inf).value.get should be(Success(expected))
      }
    }
  }
}

package scala2021.ashinkarev.task06

import scala.concurrent._
import ExecutionContext.Implicits.global

import scala2021.ashinkarev.task06.Sex._
import java.time.Duration

case class Form(
  name: String,
  age: Int, 
  email: String, 
  sex: Sex,
  height: Double
)

object FormValidator {
  def isFormValidOrError(form: Form): Either[String, Boolean] = {
    for {
      isNameValid <- NameValidator.isNameValid(form.name)
      isAgeValid <- AgeValidator.isAgeValid(form.age)
      isEmailValid <- EmailValidator.isEmailValid(form.email)
      isHeightValid <- HeightValidator.isHeightValid(form.sex, form.height)
    } yield isHeightValid
  }  
  
  def isFormValidOrAllErrors(form: Form): Either[List[String], Boolean] = {
    val errors = List(
      NameValidator.isNameValid(form.name),
      AgeValidator.isAgeValid(form.age),
      EmailValidator.isEmailValid(form.email),
      HeightValidator.isHeightValid(form.sex, form.height)
    ).collect{case Left(error) => error};

    return if (errors.isEmpty) Right(true) else Left(errors);
  }

  def isFormValidParallelOrAllErrorsAsync(form: Form): Future[Either[List[String], Boolean]] = {
    val errors = Await.result(Future.sequence(Seq(
      Future({NameValidator.isNameValid(form.name)}),
      Future({AgeValidator.isAgeValid(form.age)}),
      Future(EmailValidator.isEmailValid(form.email)),
      Future(HeightValidator.isHeightValid(form.sex, form.height)),
    )), scala.concurrent.duration.Duration("10 sec"))
    .collect{case Left(error) => error};

    return Future{
      if (errors.isEmpty) Right(true) else Left(errors.toList)
    };
  }
}

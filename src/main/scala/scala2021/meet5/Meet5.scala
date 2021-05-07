package scala2021.meet5

import cats.data.{EitherT, ValidatedNec}

import scala.concurrent.Future
import scala.util.Try
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global

object Meet5 extends App {
  def parseDouble(s: String): Either[String, Double] =
    Try(s.toDouble).map(Right(_)).getOrElse(Left(s"$s is not a number"))

  def divide(a: Double, b: Double): Either[String, Double] =
    Either.cond(b != 0, a / b, "Cannot divide by zero")

  def parseDoubleAsync(s: String): Future[Either[String, Double]] = Future.successful(parseDouble(s))

  def divideAsync(a: Double, b: Double): Future[Either[String, Double]] =
    Future.successful(divide(a, b))

  def divisionProgramAsync(inputA: String, inputB: String): Future[Either[String, Double]] =
    parseDoubleAsync(inputA) flatMap { eitherA =>
      parseDoubleAsync(inputB) flatMap { eitherB =>
        (eitherA, eitherB) match {
          case (Right(a), Right(b)) => divideAsync(a, b)
          case (Left(err), _) => Future.successful(Left(err))
          case (_, Left(err)) => Future.successful(Left(err))
        }
      }
    }


  divisionProgramAsync("10", "20").onComplete(println)

  //val res for{
  //    empl<-EitherT.fromOption[Future](employees.find(_.name....), s"Can't find employee $employee")
  //    dep <-EitherT.
  //  }
  //  empl<-EitherT.fromOption[Future](employees.find(_.name....), s"Can't find employee $employee")
  //  empl<-EitherT.fromOption[Future](employees.find(_.name....), s"Can't find employee $employee")
  //  empl<-EitherT.fromOption[Future](employees.find(_.name....), s"Can't find employee $employee")
  //  empl<-EitherT.fromOption[Future](employees.find(_.name....), s"Can't find employee $employee")
  //yield mgr.name
  def divisionProgramAsyncEitherT(inputA: String, inputB: String): EitherT[Future, String, Double] =
    for {
      a <- EitherT(parseDoubleAsync(inputA))
      b <- EitherT(parseDoubleAsync(inputB))
      result <- EitherT(divideAsync(a, b))
    } yield result

  divisionProgramAsyncEitherT("10", "20").value.onComplete(println)

  sealed trait DVal {
    def err: String
  }

  case object NameIsInvalid extends DVal {
    override def err: String = "Name is invalid"
  }

  case object AgeIsInvalid extends DVal {
    override def err: String = "Age is invalid"
  }

  type ValidationResult[A] = ValidatedNec[DVal, A]

  def valName(name: String): ValidationResult[String] = if (name.matches("^[a-z]+$")) name.validNec else NameIsInvalid.invalidNec

  def valAge(age: Int): ValidationResult[Int] = if (age > 0) age.validNec else AgeIsInvalid.invalidNec

  case class User(name: String, age: Int)

  def validateForm(name: String, age: Int): ValidationResult[User] =
    (valName(name), valAge(age)).mapN(User)

  def validateFormPar(name: String, age: Int): Future[ValidationResult[User]] = {
    val nameF = Future(valName(name))
    val ageF = Future(valAge(age))

    for {
      name <- nameF
      age <- ageF
    } yield (name, age).mapN(User)
  }

  println(validateForm("user", 9))
  println(validateForm("user", 0))


}

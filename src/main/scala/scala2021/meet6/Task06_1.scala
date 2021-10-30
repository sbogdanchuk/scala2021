package scala2021

//erusak
package meet6 {


  import scala2021.meet6.SexType.SexType

  import scala.concurrent.{ExecutionContext, Future}

  object SexType extends Enumeration {
    type SexType = Value
    val Male, Female = Value
  }

  sealed trait ValidationError {
    def cause: String
  }

  final case class Name(value: String) extends AnyVal

  final case class Age(value: Int) extends AnyVal

  final case class Email(value: String) extends AnyVal

  final case class Height(value: Double) extends AnyVal

  final case class Contract(name: Name, age: Age, email: Email, sex: SexType, height: Height)


  trait Validate {
    def validateFirstFail(contract: Contract): Either[String, Contract]

    def validateCollectAll(contract: Contract): Either[List[String], Contract]

    def validateAsync(contract: Contract)(implicit executor: ExecutionContext): Future[Either[List[String], Contract]]
  }

  object Validate {

    private[meet6] def nameValidation(input: Contract): Either[String, Contract] = input.name.value match {
      case name if name.isEmpty => Left("Name should not be empty")
      case name if !name.matches("[A-Za-z]+") => Left("Name should have only Latin characters")
      case _ => Right(input)
    }

    private[meet6] def ageValidation(input: Contract): Either[String, Contract] = input.age.value match {
      case age if age < 0 => Left("Age cannot be negative")
      case age if age > 100 => Left("Age cannot be greater than 100")
      case _ => Right(input)
    }

    private[meet6] def emailValidation(input: Contract): Either[String, Contract] = input.email.value match {
      case email if email.isEmpty => Right(input)
      case email if !email.matches(".+\\@.+\\..+") => Left("Email is not valid")
      case _ => Right(input)
    }

    private[meet6] def sexValidation(input: Contract): Either[String, Contract] = input.sex match {
      case SexType.Male if input.height.value <= 100.0 => Left("Male's height should be greater than 100")
      case _ => Right(input)
    }

    def apply(blocks: List[Function[Contract, Either[String, Contract]]]): Validate = new DefaultValidation(blocks)

  }

  private final class DefaultValidation(val validators: List[Function[Contract, Either[String, Contract]]]) extends Validate {

    override def validateFirstFail(contract: Contract): Either[String, Contract] =
      validateCollectAll(contract).left.map(_.head)

    override def validateCollectAll(contract: Contract): Either[List[String], Contract] =
      validateCollect(validators.map(func => func(contract)))

    override def validateAsync(contract: Contract)(implicit executor: ExecutionContext): Future[Either[List[String], Contract]] = {
      val futures = validators.map(func => Future {
        func(contract)
      })
      Future.sequence(futures).collect(result => validateCollect(result))
    }

    private def validateCollect(validateResults: List[Either[String, Contract]]): Either[List[String], Contract] = {
      validateResults.partition(_.isLeft) match {
        case (Nil, contracts) => Right((for (Right(contract) <- contracts) yield contract).head)
        case (errors, _) => Left(for (Left(error) <- errors) yield error)
      }
    }
  }

}

package scala2021.meet6

import cats.implicits._
import cats.data.EitherT

import scala.concurrent.Future
import scala.concurrent._
import ExecutionContext.Implicits.global

//sbahdanchuk
object EmployeeRepository {

  /*
    Given You have collections imitating repositories with db access.
    Employees, Departments, Managers - are corresponding collections

    While completing this task, pay attention on errors handling and data types like Option and Either.
    Use for-comprehension concept, and compare it with traditional for-loop in imperative languages.
   */
  case class Employee(id: Int, name: String, departmentId: Int)

  case class Department(id: Int, name: String)

  case class Manager(department: String, employeeId: Int)

  case class Info(employee: String, departmment: String, manager: String)

  val employees = List(
    Employee(1, "Steve", 1),
    Employee(3, "Mark", 1),
    Employee(4, "Jane", 1),
    Employee(7, "Samuel", 2),
    Employee(10, "Igor", 2),
    Employee(11, "Naveen", 4),
    Employee(12, "Christy", 5),
    Employee(15, "Megan", 3)
  )
  val departments = List(
    Department(1, "Marketing"),
    Department(2, "Sales"),
    Department(3, "Research"),
    Department(4, "IT"),
  )
  val managers = List(
    Manager("Marketing", 1),
    Manager("Sales", 10),
    Manager("IT", 14),
  )

  /*
    Find manager name by employee name
   */
  def findManagerName(employee: String): Option[String] = {
    employees.find(_.name == employee)
      .flatMap(e => departments.find(_.id == e.departmentId))
      .flatMap(dep => managers.find(_.department == dep.name))
      .flatMap(e => employees.find(_.id == e.employeeId))
      .map(e => e.name)
  }

  def findManagerNameFor(employee: String): Option[String] = {
    val res = for {
      empl <- employees if empl.name == employee
      dep <- departments if dep.id == empl.departmentId
      mgr <- managers if mgr.department == dep.name
      mgrEmpl <- employees if mgrEmpl.id == mgr.employeeId
    } yield mgrEmpl.name
    res.headOption
  }

  /*
    Find manager name by employee name, in case of error in data - return message with description of error
   */
  def findManagerNameOrError(employee: String): Either[String, String] = for {
    empl <- employees.find(_.name == employee).toRight(s"Can't find employee $employee")
    dep <- departments.find(_.id == empl.departmentId).toRight(s"Can't find department ${empl.departmentId} for employee ${empl.id}")
    mgr <- managers.find(_.department == dep.name).toRight(s"Can't find manager for department $dep")
    mgrEmpl <- employees.find(_.id == mgr.employeeId).toRight(s"Can't find employee ${mgr.employeeId} who should be manager for department ${dep.name}")
  } yield mgrEmpl.name


  /*
    Find manager name by employee name, in case of error in data return message with description of error. Do it asynchronously
   */
  def findManagerNameOrErrorAsync(employee: String): Future[Either[String, String]] = {
    val res = for {
      empl <- EitherT.fromOption[Future](employees.find(_.name == employee), s"Can't find employee $employee")
      dep <- EitherT.fromOption[Future](departments.find(_.id == empl.departmentId), s"Can't find department ${empl.departmentId} for employee ${empl.id}")
      mgr <- EitherT.fromOption[Future](managers.find(_.department == dep.name), s"Can't find manager for department $dep")
      mgrEmpl <- EitherT.fromOption[Future](employees.find(_.id == mgr.employeeId), s"Can't find employee ${mgr.employeeId} who should be manager for department ${dep.name}")
    } yield mgrEmpl.name
    res.value
  }

  /*
    Return list of all employees, with department name and manager's name, if department or manager is not exists, then use constant "Not Found"
   */
  def findEmployeeManagers: List[Info] = {
    employees.map { e =>
      val dep = departments.find(_.id == e.departmentId) match {
        case Some(dep) => Right(dep, managers.find(_.department == dep.name))
        case None => Left(("Not Found", "Not Found"))
      }
      val andMgr = dep match {
        case Right((dep, mgrOpt)) => Right(e, dep, mgrOpt.flatMap(mgr => employees.find(_.id == mgr.employeeId)))
        case Left((depNotFound, mgrNotFound)) => Left(e, depNotFound, mgrNotFound)
      }

      andMgr match {
        case Right((empl, dep, Some(mgr))) => Info(empl.name, dep.name, mgr.name)
        case Right((empl, dep, None)) => Info(empl.name, dep.name, "Not Found")
        case Left((empl, depNotFound, mgrNotFound)) => Info(empl.name, depNotFound, mgrNotFound)
      }
    }
  }
}

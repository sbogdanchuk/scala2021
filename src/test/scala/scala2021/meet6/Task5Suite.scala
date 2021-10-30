package scala2021.meet6

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import scala2021.meet6.EmployeeRepository._

import scala.concurrent._
import ExecutionContext.Implicits.global

class Task5Suite extends AnyFunSuite with TableDrivenPropertyChecks with Matchers {
  test("sanity check") {
    2 + 3 should be(5)
  }

  val emplMgrName = Table(
    ("emplName", "expectedMgrName"),
    ("Steve", Some("Steve")),
    ("Mark", Some("Steve")),
    ("Jane", Some("Steve")),
    ("Samuel", Some("Igor")),
    ("Igor", Some("Igor")),
    ("Naveen", None),
    ("Christy", None),
    ("Megan", None),
    ("", None)
  )

  test("check find manager name") {
    forAll(emplMgrName) {
      (emplName, expectedMgrName) => {
        findManagerName(emplName) should be(expectedMgrName)
      }
    }
  }

  test("check find manager name for-comprehension") {
    forAll(emplMgrName) {
      (emplName, expectedMgrName) => {
        findManagerNameFor(emplName) should be(expectedMgrName)
      }
    }
  }

  def errMsg(emplId: String, dep:String): String = s"Can't find employee $emplId who should be manager for department $dep"

  val emplMgrNameEither = Table(
    ("emplName", "expectedMgrName"),
    ("Steve", Right("Steve")),
    ("Mark", Right("Steve")),
    ("Jane", Right("Steve")),
    ("Samuel", Right("Igor")),
    ("Igor", Right("Igor")),
    ("Naveen", Left("Can't find employee 14 who should be manager for department IT")),
    ("Christy", Left("Can't find department 5 for employee 12")),
    ("Megan", Left("Can't find manager for department Department(3,Research)")),
    ("", Left("Can't find employee "))
  )

  test("check find manager name or err") {
    forAll(emplMgrNameEither) {
      (emplName, expectedMgrName) => {
        findManagerNameOrError(emplName) should be(expectedMgrName)
      }
    }
  }

  test("check find manager name or err async") {
    forAll(emplMgrNameEither) {
      (emplName, expectedMgrName) => {
        findManagerNameOrErrorAsync(emplName)
          .foreach(e => e should be(expectedMgrName))
      }
    }
  }

  val employeeInfo = Set(
    Info("Steve", "Marketing", "Steve"),
    Info("Mark", "Marketing", "Steve"),
    Info("Jane", "Marketing", "Steve"),
    Info("Samuel", "Sales", "Igor"),
    Info("Igor", "Sales", "Igor"),
    Info("Naveen", "IT", "Not Found"),
    Info("Christy", "Not Found", "Not Found"),
    Info("Megan", "Research", "Not Found"),
  )

  test("check findEmployeeManagers return correct data"){
    val actual = findEmployeeManagers.toSet

    actual should be (employeeInfo)
  }
}

package scala2021.sbahdanchuk.task01

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class HWSuite extends AnyFunSuite with TableDrivenPropertyChecks with ScalaCheckDrivenPropertyChecks with Matchers {

  import HW.add

  test("check add 2 and 3 should give 5") {
    assert(add(2, 3) === 5)
  }


  val tblAddData = Table(
    ("a", "b", "expected"),
    (2, 3, 5),
    (0, 2, 2),
    (-1, 0, -1)
  )

  test("check add correctly (table)") {
    forAll(tblAddData) {
      (a, b, expected) => {
        add(a, b) should be(expected)
      }
    }
  }

  case class AddData(a: Int, b: Int, expected: Int)

  def genAddData = for {
    a <- Gen.choose(-1000000, 1000000)
    b <- Gen.choose(-1000000, 1000000)

  } yield AddData(a, b, a + b)

  implicit val arbGenAddItem: Arbitrary[AddData] = Arbitrary(genAddData)

  test("check add generated should be correct") {
    forAll {
      (item: AddData) => {
        add(item.a, item.b) should be(item.expected)
      }
    }
  }
}

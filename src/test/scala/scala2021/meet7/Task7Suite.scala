package scala2021.meet7

import com.typesafe.scalalogging.StrictLogging
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.Success
import org.scalatest.TryValues._
import org.scalatest.matchers.must.Matchers.have
import scala2021.meet7.Task7.Connection

import java.io.File

//erusak
class Task7Suite extends AnyFlatSpec with StrictLogging {
  private val TaskId = "Task 07 (Control Structure)"

  private val connRun: Connection => Unit = conn => conn.run()
  private val fileExists: File => Boolean = file => file.exists()

  it should s"$TaskId properly handle Connection resource" in {

    Task7.withConnection(port = 9000) {
      connRun
    } shouldBe Success(())

    Task7.withConnection(port = 9001) {
      connRun
    }.failure.exception should have message "Issue during connection init"

    Task7.withConnection(port = 9002) {
      connRun
    }.failure.exception should have message "Issue during connection run"

    Task7.withConnection(port = 9003) {
      connRun
    }.failure.exception should have message "Issue during connection close"

  }

  it should s"$TaskId properly handle File resource" in {

    Task7.withFile(path = "/tmp/") {
      fileExists
    } shouldBe Success(true)

    Task7.withFile(path = "/tmp/sdfsdfsdfs") {
      fileExists
    } shouldBe Success(false)

  }

  it should s"$TaskId properly handle FileInputStream resource" in {

    import java.io.File
    val tempFile = File.createTempFile("scalatest-", "-exadel")

    Task7.withFileInputStream(path = tempFile.getPath) {
      fis => {
        val bytes = fis.available()
        logger.info(s"Count bytes are $bytes")
        bytes
      }

    } shouldBe Success(0)

    tempFile.deleteOnExit()

  }

}


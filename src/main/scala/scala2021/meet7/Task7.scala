package scala2021.meet7

import com.typesafe.scalalogging.StrictLogging

import java.io.{File, FileInputStream}
import scala.util.{Failure, Try}

//erusak
object Task7 extends StrictLogging {

  /** Loan pattern to init, operate and release resource if needed. Resource will be tried to be closed
   *
   * @param initResource    resource initialization logic
   * @param op              operation to perform with the resource
   * @param releaseResource how to release the resource, default is noop
   * @tparam T resource type
   * @tparam R return type
   * @return a [[Try]] containing the exception is something happens during initResource, op or releaseResource
   *         Note that even if the exception happens during releaseResource the result of op will be ignored
   */
  private[meet7] def withResource[T, R](initResource: => T, releaseResource: T => Unit = (_: T) => ())(op: T => R): Try[R] = {
    val tryResource = Try {
      initResource
    }
    val tryResult = for (resource <- tryResource) yield op(resource)
    val tryClose = for (resource <- tryResource) yield releaseResource(resource)

    tryClose.transform(_ => tryResult, f => Failure(f))

  }

  def withFile[R](path: String)(op: File => R): Try[R] =
    withResource[File, R](new File(path)) {
      op
    }

  def withFileInputStream[R](path: String)(op: FileInputStream => R): Try[R] =
    withResource[FileInputStream, R](new FileInputStream(path), fis => fis.close()) {
      op
    }

  def withConnection[R](port: Int)(op: Connection => R): Try[R] =
    withResource[Connection, R](Connection(port = port), connection => connection.close()) {
      op
    }

  sealed trait Connection {
    val port: Int

    def close(): Unit

    def run(): Unit
  }

  object Connection {
    def apply(port: Int): Connection = port match {
      case 9001 => ConnectionWithExceptionOnInit(port = port)
      case 9002 => ConnectionWithExceptionOnRun(port = port)
      case 9003 => ConnectionWithExceptionOnClose(port = port)
      case _ => ConnectionSuccess(port = port)
    }
  }

  private[meet7] final case class ConnectionSuccess(port: Int) extends Connection {
    def close(): Unit = logger.info("Closed")

    def run(): Unit = {
      logger.info("Run")
      Thread.sleep(2_000)
    }
  }

  private[meet7] final case class ConnectionWithExceptionOnInit(port: Int) extends Connection {

    throw new IllegalArgumentException("Issue during connection init")

    def close(): Unit = logger.info("Closed")

    def run(): Unit = {
      logger.info("Run")
    }
  }

  private[meet7] final case class ConnectionWithExceptionOnRun(port: Int) extends Connection {

    def close(): Unit = logger.info("Closed")

    def run(): Unit = {
      logger.info("Run")
      Thread.sleep(2_000)
      throw new IllegalArgumentException("Issue during connection run")
    }
  }

  private[meet7] final case class ConnectionWithExceptionOnClose(port: Int) extends Connection {

    def close(): Unit = {
      logger.info("Closed")
      throw new IllegalArgumentException("Issue during connection close")
    }

    def run(): Unit = {
      logger.info("Run")
      Thread.sleep(2_000)
    }
  }

}


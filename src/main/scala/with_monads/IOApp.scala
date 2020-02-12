package with_monads

sealed abstract case class ExitCode(value: Int)
object Success extends ExitCode(0)
object Failure extends ExitCode(1)

abstract class IOApp {
  def run(args: Array[String]): IO[ExitCode]

  def main(args: Array[String]): Unit = {
    System.exit(run(args).unsafeRunSync().value)
  }
}
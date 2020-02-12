package with_monads

object Main extends IOApp {
  def run(args: Array[String]): IO[ExitCode] = {
    IO.delay(println("Hello IO!"))
      .map(_ => Success)
  }
}
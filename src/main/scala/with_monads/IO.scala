package with_monads

sealed abstract class IO[A]

object IO {

  case class Pure[A](a: A) extends IO[A]
  case class FlatMap[B,A](fa: IO[B], f: B => IO[A]) extends IO[A]
  case class Delay[A](f: () => A) extends IO[A] // Can we pass down Math.random() or just Math.random  ?

  def unsafeRunSync[A](io: IO[A]): A = {
    io match {
      case Delay(f) => f()
      case Pure(a) => a
      case FlatMap(fa, f) => {
        val a = unsafeRunSync(fa)
        unsafeRunSync(f(a))
      }
    }
  }

  def pure[A](a: A): IO[A] = Pure(a)
  def delay[A](f: => A): IO[A] = Delay(() => f)
  def flatMap[B,A](fa: IO[B], f: B => IO[A]): IO[A] = FlatMap(fa, f)

  implicit val monadForIO: Monad[IO[*]] = new Monad[IO[*]]  {
    def flatMap[A, B](fa: IO[A])(g: A => IO[B]): IO[B] = FlatMap(fa, g)
    def pure[A](a: A): IO[A] = Pure(a)
  }
}

trait IOOps {
  final class IOSyntax[A](ioa: IO[A]) {
    def unsafeRunSync() = IO.unsafeRunSync(ioa)
  }

  implicit def syntaxForIO[A](ioa: IO[A]) = new IOSyntax[A](ioa)
}
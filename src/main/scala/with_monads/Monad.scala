package with_monads
 
trait Monad[F[_]] extends Functor[F] {
  def flatMap[A, B](fa: F[A])(g: A => F[B]): F[B]
  def pure[A](a: A): F[A]
  def map[A,B](fa: F[A])(g: A => B): F[B] = flatMap(fa)(a => pure(g(a)))
}

object Monad {
  def apply[M[_]](implicit instance: Monad[M]): Monad[M] = instance

  implicit def monadForOption: Monad[Option] = new Monad[Option] {
    def flatMap[A,B](fa: Option[A])(g: A => Option[B]): Option[B] = {
      fa match {
        case None    => None
        case Some(a) => g(a)
      }
    }
    def pure[A](a: A): Option[A] = Some(a)
  }

  implicit def monadForList: Monad[List] = new Monad[List] {
    def flatMap[A,B](fa: List[A])(g: A => List[B]): List[B] = {
      fa match {
        case Nil => Nil
        case hd :: tail => g(hd) ++ flatMap(tail)(g)
      }
    }
    def pure[A](a: A): List[A] = List(a)
  }

  implicit def monadForEither[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
    def flatMap[A,B](fa: Either[E,A])(g: A => Either[E,B]): Either[E,B] = {
      fa match {
        case Left(e) => Left(e)
        case Right(a) => g(a)
      }
    }
    def pure[A](a: A): Either[E,A] = Right(a)
  }
}

trait MonadOps {
  final class MonadSyntax[M[_]: Monad, A](ma: M[A]) {
    val M = implicitly[Monad[M]]
    def map[B](g: A => B): M[B] = M.map(ma)(g)
    def flatMap[B](g: A => M[B]): M[B] = M.flatMap(ma)(g)
    def pure[A](a: A): M[A] = M.pure(a)
  }

  implicit def syntax[M[_]: Monad, A](ma: M[A]) = new MonadSyntax[M,A](ma)
}

object MonadOps extends MonadOps
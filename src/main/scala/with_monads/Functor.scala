package with_monads

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(g: A => B): F[B]
}

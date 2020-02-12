package with_monads

trait Foldable[F[_]] {
  def foldLeft[A,B](fa: F[A], b: B)(f: (B,A) => B): B
  def foldRight[A,B](fa: F[A], lb: B)(f: (A,B) => B): B
}
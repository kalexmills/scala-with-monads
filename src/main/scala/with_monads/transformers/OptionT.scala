package with_monads.transformers

import with_monads._

case class OptionT[F[_]: Monad, A](value: F[Option[A]]) {
  def map[B](f: A => B): OptionT[F, B] =
    OptionT(value.map(_ match {
      case None => None
      case Some(a) => Some(f(a))
    }))

  def flatMap[B](f: A => OptionT[F, B]): OptionT[F,B] ={
    OptionT(value.flatMap(option => option match {
      case None => Monad[F].pure(None)
      case Some(a) => f(a).value
    }))
  }
}

object OptionT {
  implicit def monadForOptionT[F[_]: Monad]: Monad[OptionT[F, *]] = new Monad[OptionT[F, *]] {
    def flatMap[A, B](fa: OptionT[F,A])(f: A => OptionT[F,B]): OptionT[F,B] = fa.flatMap(f)
    def pure[A](x: A): OptionT[F,A] = OptionT(Monad[F].pure(Some(x)))
  }

  def fromOption[F[_]: Monad, A](o: Option[A]): OptionT[F, A] = OptionT(Monad[F].pure(o))
  def liftF[F[_]: Monad, A](o: F[A]): OptionT[F, A] = OptionT(o.map(Some(_)))
}
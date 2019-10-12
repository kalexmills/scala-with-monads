package with_monads.transformers

import with_monads._

final case class EitherT[F[_]: Monad, E, A](value: F[Either[E, A]]) {
    def map[B](f: A => B): EitherT[F, E, B] = EitherT(value.map(_.map(f)))

    def flatMap[B](f: A => EitherT[F, E, B]): EitherT[F, E, B] = 
        EitherT(value.flatMap(either => either.fold(EitherT.left[F, E, B], f).value))

}

object EitherT {
    def left[F[_]: Monad, E, A](e: E): EitherT[F, E, A] = EitherT(Monad[F].pure(Left(e)))

    def pure[F[_]: Monad, E, A](a: A): EitherT[F, E, A] = EitherT(Monad[F].pure(Right(a)))

    implicit def monadForEitherT[F[_]: Monad, E]: Monad[EitherT[F, E, *]] = new Monad[EitherT[F, E, *]] {
        override def flatMap[A, B](fa: EitherT[F,E,A])(g: A => EitherT[F,E,B]): EitherT[F,E,B] = fa.flatMap(g)
        override def map[A, B](fa: EitherT[F,E,A])(g: A => B): EitherT[F,E,B] = fa.map(g)
        override def pure[A](a: A): EitherT[F,E,A] = EitherT(Monad[F].pure(Right(a)))
    }
}
package with_monads.transformers

import with_monads._

final case class StateT[F[_]: Monad, S, A](run: S => F[(S, A)]) {
    def runA(s: S): F[A] = run(s).map(_._2)

    def map[B](f: A => B): StateT[F, S, B] = 
        StateT((s) => run(s).map({ case (s, a) => (s, f(a)) }))

    def flatMap[B](f: A => StateT[F, S, B]): StateT[F, S, B] = 
        StateT((s) => run(s).flatMap({ case (s, a) => f(a).run(s) }))
}

object StateT {
    def pure[F[_]: Monad, S, A](a: A): StateT[F, S, A] = StateT((s) => Monad[F].pure((s, a)))

    def get[F[_]: Monad, S]: StateT[F, S, S] = StateT((s) => Monad[F].pure(s -> s))
    def inspect[F[_]: Monad, S, A](f: S => A): StateT[F, S, A] = StateT((s) => Monad[F].pure(s -> f(s)))
    def set[F[_]: Monad, S](s: S): StateT[F, S, Unit] = StateT((_ => Monad[F].pure(s -> ())))
    def modify[F[_]: Monad, S](f: S => S): StateT[F, S, S] = StateT(s => {
        val next = f(s)
        Monad[F].pure(next -> next)
    })

    def liftF[F[_]: Monad, S, A](fa: F[A]): StateT[F, S, A] = StateT((s) => fa.map(s -> _))

    implicit def monadForStateT[F[_]: Monad, S]: Monad[StateT[F, S, *]] = new Monad[StateT[F, S, *]] {
        def flatMap[A, B](fa: StateT[F,S,A])(g: A => StateT[F,S,B]): StateT[F,S,B] = fa.flatMap(g)
        override def map[A, B](fa: StateT[F,S,A])(g: A => B): StateT[F,S,B] = fa.map(g)
        def pure[A](a: A): StateT[F,S,A] = StateT.pure[F, S, A](a)
    }
}
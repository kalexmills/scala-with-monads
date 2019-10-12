package with_monads.transformers

import with_monads._

final case class StateT[F[_]: Monad, S, A](run: S => F[(S, A)]) {
    def map[B](f: A => B): StateT[F, S, B] = 
        StateT((s) => run(s).map({ case (s, a) => (s, f(a)) }))

    def flatMap[B](f: A => StateT[F, S, B]): StateT[F, S, B] = 
        StateT((s) => run(s).flatMap({ case (s, a) => f(a).run(s) }))
}

object StateT {
    def pure[F[_]: Monad, S, A](a: A): StateT[F, S, A] = StateT((s) => Monad[F].pure((s, a)))

    implicit def monadForStateT[F[_]: Monad, S]: Monad[StateT[F, S, *]] = new Monad[StateT[F, S, *]] {
        def flatMap[A, B](fa: StateT[F,S,A])(g: A => StateT[F,S,B]): StateT[F,S,B] = fa.flatMap(g)
        override def map[A, B](fa: StateT[F,S,A])(g: A => B): StateT[F,S,B] = fa.map(g)
        def pure[A](a: A): StateT[F,S,A] = StateT.pure[F, S, A](a)
    }
}
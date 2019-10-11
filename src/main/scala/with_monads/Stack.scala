package with_monads

object Stack {
    def push[A](a: A): State[List[A], Unit] = State.modify[List[A]](a :: _).map(_ => ())
    def pop[A]: State[List[A], Option[A]] = 
        State.get[List[A]].map(_.headOption)
            .flatMap(head => State.modify[List[A]](_.drop(1)).map(_ => head))

    def peek[A]: State[List[A], Option[A]] = State.get[List[A]].map(_.headOption)

    def size[A]: State[List[A], Int] = State.get[List[A]].map(_.size)
}
package object with_monads extends MonadOps with IOOps {
  import cats.Id

  // TODO: figure out how MonadOps needs to be translated???
  //type Reader[C,B] = ReaderT[Id, C, B]
  //type State[S,A] = StateT[Id, S, A]
}

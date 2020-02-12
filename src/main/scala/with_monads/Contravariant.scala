package with_monads

trait Contravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

object Contravariant {
  // Data types
  // Option[A] "==="  () => A     (.get(): A)
  // List[A]   "==="  (Int) => A  (.get(Int): A)

  // "Function Types"
  // Function[A,B]   "==="  A  =>  B   .contramap(C => A)  C => B
  // Encoder[B]      (B) => Json

  implicit def contravariantForFunction1[C]: Contravariant[Function1[*, C]] =
    new Contravariant[Function1[*, C]] {
      def contramap[A, B](fa: Function1[A, C])(f: B => A): Function[B, C] =
        f andThen (fa)
    }
}

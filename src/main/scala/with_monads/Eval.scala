package with_monads
import with_monads.Eval.Pure
import with_monads.Eval.Delay
import with_monads.Eval.FlatMap
import scala.collection.immutable.Nil

sealed trait Eval[A] extends Product with Serializable {
  import Eval._

  final def value: A = run(this)

  final def map[B](g: A => B): Eval[B] = flatMap((a) => pure(g(a)))

  final def flatMap[B](g: A => Eval[B]): Eval[B] = FlatMap(this, g)

}

object Eval {
  final case class Pure[A](a: A) extends Eval[A]
  final case class Delay[A](a: () => Eval[A]) extends Eval[A]
  final case class FlatMap[L, A](left: Eval[L], f: L => Eval[A]) extends Eval[A]

  def pure[A](a: A): Eval[A] = Pure(a)
  def delay[A](a: => Eval[A]): Eval[A] = Delay(() => a)
  def suspend[A](a: => A): Eval[A] = Delay(() => pure(a))

  def run[A](eval: Eval[A]): A = {
    var current: Eval[Any] = eval.asInstanceOf[Eval[Any]]
    var result: Option[Any] = None
    var stack: List[Any => Eval[Any]] = List.empty

    def step(a: Any): Eval[Any] = {
      stack match {
        case head :: tl =>
          stack = tl
          head(a)
        case Nil =>
          result = Some(a)
          pure(a)
      }
    }

    while (result.isEmpty) {
      current match {
        case Pure(a) =>
          current = step(a)
        case Delay(a) =>
          current = a()
        case FlatMap(left, f) => 
          stack = f :: stack
          current = left
      }
    }

    result.get.asInstanceOf[A]
  }

  def factorial_(n: BigInt): BigInt =
    if (n == 1 || n == 0) BigInt(1)
    else n * factorial_(n - 1)

  def factorial(n: BigInt): Eval[BigInt] =
    if (n <= 1) suspend(BigInt(1))
    // we use delay here because it is important to break the continual recursion
    else delay(factorial(n - 1)).map(_ * n)
}

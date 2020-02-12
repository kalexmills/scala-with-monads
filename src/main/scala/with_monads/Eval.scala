package with_monads

import Eval._
sealed trait Eval[A] {
  def value: A = Eval.run(this)
  def map[B](g: A => B) = Eval.flatMap(this)(a => pure(g(a)))
  def flatMap[B](g: A => Eval[B]): Eval[B] = Eval.flatMap(this)(g)
}

object Eval {
  case class Pure[A](a: A) extends Eval[A]
  case class Delay[A](a: () => Eval[A]) extends Eval[A]
  case class FlatMap[A,B](ea: Eval[A], g: A => Eval[B]) extends Eval[B]

  def pure[A](a: A): Eval[A] = new Pure(a)
  def delay[A](a: => Eval[A]): Eval[A] = new Delay(() => a)
  def flatMap[A,B](ea: Eval[A])(g: A => Eval[B]): Eval[B] = new FlatMap(ea,g)

  // Eval.run(Eval.flatMap(Eval.delay(Eval.pure(12)), x => Eval.Pure(x * 12)))

  def run[A](e: Eval[A]): A = {
    var current: Eval[Any] = e.asInstanceOf[Eval[Any]]
    var result: Option[Any] = None
    var stack: List[Any => Eval[Any]] = List.empty

    def step(a: Any): Eval[Any] = {
      stack match {
        case head :: tl => {
          stack = tl
          head(a)
        }
        case Nil => {
          result = Some(a)
          pure(a)
        }
      }
    }

    while (result.isEmpty) {
      current match {
        case Pure(a) =>
          current = step(a)
        case Delay(a) =>
          current = a()
        case FlatMap(ea, g) =>
          stack = g :: stack
          current = ea
      }
    }
    result.get.asInstanceOf[A]
  }

  implicit def monadForEval: Monad[Eval] = new Monad[Eval] {
    def flatMap[A, B](fa: Eval[A])(g: A => Eval[B]): Eval[B] = new FlatMap(fa, g)
    def pure[A](a: A): Eval[A] = pure(a)
  }
}
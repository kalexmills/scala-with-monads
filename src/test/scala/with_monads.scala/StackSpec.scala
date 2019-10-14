package with_monads.scala

import cats.Monoid
import cats.implicits._

import with_monads._
import with_monads.Writer

import org.scalatest._
import org.scalatest.Matchers._

class StackSpec extends FlatSpec {

  def push[A](v: A) = State[List[A], Unit] { s =>
    (v :: s, Unit)
  }

  def pop[A]: State[List[A], Option[A]] = State { (s: List[A]) =>
    s match {
      case Nil    => (s, None)
      case h :: t => (t, Some(h))
    }
  }

  def popPopPush[A](implicit ev: Monoid[A]): State[List[A], A] = for {
    x <- pop.map(_.getOrElse(Monoid.empty))
    y <- pop.map(_.getOrElse(Monoid.empty))
    _ <- push(Monoid.combine(x, y))
  } yield (Monoid.combine(x, y))
  
  "Stack" should "be (List(3, 3), 3) for [x = pop(), y = pop(), push(x+y)] on List(1, 2, 3)" in {
    popPopPush.run(List(1, 2, 3)) should equal(List(3, 3), 3)
  }

  it should "be (List(0), 0) for [x = pop(), y = pop(), push(x+y)] on List.empty" in {
    popPopPush.run(List.empty) should equal(List(0), 0)
  }
}

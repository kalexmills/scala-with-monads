package with_monads.scala

import cats.Monoid
import cats.implicits._

import with_monads._
import with_monads.Writer

import org.scalatest._
import org.scalatest.Matchers._

class StackSpec extends FlatSpec {

  def push(v: Int) = State[List[Int], Unit] { s =>
    (v :: s, Unit)
  }

  val pop: State[List[Int], Option[Int]] = State { (s: List[Int]) =>
    s match {
      case Nil    => (s, None)
      case h :: t => (t, Some(h))
    }
  }

  val popPopPush: State[List[Int], Int] = for {
    x <- pop.map(_.getOrElse(0))
    y <- pop.map(_.getOrElse(0))
    _ <- push(x + y)
  } yield (x + y)
  
  "Stack" should "be (List(3, 3), 3) for [x = pop(), y = pop(), push(x+y)] on List(1, 2, 3)" in {
    popPopPush.run(List(1, 2, 3)) should equal(List(3, 3), 3)
  }

  it should "be (List(0), 0) for [x = pop(), y = pop(), push(x+y)] on List.empty" in {
    popPopPush.run(List.empty) should equal(List(0), 0)
  }
}

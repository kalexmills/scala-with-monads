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

  "Stack" should "yield expected results for [x = pop(), y = pop(), push(x+y)]" in {
    val q: State[List[Int], Int] = for {
      x <- pop.map(_.getOrElse(0))
      y <- pop.map(_.getOrElse(0))
      _ <- push(x + y)
    } yield (x + y)

    q.run(List(1, 2, 3)) should equal(List(3, 3), 3)
    q.run(List.empty) should equal(List(0), 0)
  }
}

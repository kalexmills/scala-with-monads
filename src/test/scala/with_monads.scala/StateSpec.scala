package with_monads.scala

import cats.implicits._

import with_monads._

import org.scalatest._
import org.scalatest.Matchers._

class StateSpec extends FlatSpec {
  "State" should "allow user to implement a stack" in {
    def pushOp(toPush: Int): State[List[Int], Unit] = State[List[Int],Unit] {
      (list) => (toPush :: list, ())
    }
    def popOp: State[List[Int], Option[Int]] = State[List[Int], Option[Int]] {
      (list) => (list.tail, list.headOption)
    }
    State.monadForState[List[Int]].pure(1)

    val composed = for {
      x <- popOp.map(_.getOrElse(0))
      y <- popOp.map(_.getOrElse(0))
      _ <- pushOp(x + y)
    } yield (x+y)

    composed.run(List(2,3,4,5)) should equal (List(5,4,5), 5)
  }
}
package with_monads

import parser._, Parser._

object Calc {
  lazy val atom: Parser[Int] = delay(paren(low) or int)

  def paren[A](p: Parser[A]): Parser[A] = char('(') >> p flatTap(_ => char(')'))

  lazy val high: Parser[Int] = {
    val single = atom
    val expr = for {
    left <- atom
    op <- char('*') or char('/')
    right <- high
  } yield if (op == '*') left * right else left / right
  expr or single
}

  lazy val low: Parser[Int] = {
    val single = high
    val expr = for {
      left <- high
      op <- char('+') or char('-')
      right <- low
    } yield if (op == '+') left + right else left - right

    expr or single
  }

  lazy val calc = low flatTap(_ => eof)
}
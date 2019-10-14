package with_monads

import transformers._
import parser._, Parser._

object Calc {
  lazy val atom: OptionT[Parser, Int] = OptionT(delay(paren(low.value) or int.map(Some(_): Option[Int])))

  def paren[A](p: Parser[A]): Parser[A] =
    char('(') >> p flatTap (_ => char(')'))

  lazy val high: OptionT[Parser, Int] = {
    val single = atom
    val expr: OptionT[Parser, Int] = for {
      left <- atom
      op <- OptionT.liftF[Parser, Char](token(char('*') or char('/')))
      right <- high
      r <- if (op == '*') OptionT.pure(left * right)
      else if (right != 0) OptionT.pure(left / right)
      else OptionT.none
    } yield r
    OptionT(expr.value or single.value)
  }

  lazy val low: OptionT[Parser, Int] = {
    val single = high
    val expr: OptionT[Parser, Int] = for {
      left <- high
      op <- OptionT.liftF(token(char('+') or char('-')))
      right <- low
    } yield if (op == '+') left + right else left - right

    OptionT(expr.value or single.value)
  }

  lazy val calc = low.value <* eof
}

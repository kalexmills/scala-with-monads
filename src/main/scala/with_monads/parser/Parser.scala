package with_monads.parser

import with_monads._
import with_monads.transformers._
import scala.util.matching.Regex

final class Parser[A](private val parser: Parser.M[A]) {
  def parse(s: String): Either[Error, A] = parser.value.runA(S(0, s)).value

  def map[B](f: A => B): Parser[B] = Parser(parser.map(f))

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    Parser(parser.flatMap(a => f(a).parser))
}

object Parser {
  type Inner[A] = StateT[Eval, S, A]
  type M[A] = EitherT[StateT[Eval, S, *], Error, A]

  def apply[A](parse: EitherT[StateT[Eval, S, *], Error, A]): Parser[A] =
    new Parser(parse)

  def char(c: Char): Parser[Char] =
    Parser(for {
      s <- getS
      _ <- if (s.offset >= s.string.length || s.string.charAt(s.offset) != c)
        fail(Error(s"expected $c", s.offset))
      else Monad[M].unit
      _ <- advance(1)
    } yield c)

  def anyChar(c: Char): Parser[Char] =
    Parser(for {
      s <- getS
      c <- if (s.offset >= s.string.length)
        fail(Error(s"expected any char", s.offset))
      else Monad[M].pure(s.string.charAt(s.offset))
      _ <- advance(1)
    } yield c)
  
  def string(str: String): Parser[String] = 
    Parser(for {
      s <- getS
      _ <- if (s.string.slice(s.offset, str.length) != str)
        fail(Error(s"expected $str", s.offset))
      else Monad[M].unit
      _ <- advance(str.length)
    } yield str)

  def regex(re: Regex): Parser[String] = 
    Parser(for {
      s <- getS
      m <- re.findPrefixMatchOf(s.string).fold(fail[String](Error(s"expected $re", s.offset)))(m => Monad[M].pure(m.matched))
      _ <- advance(m.length)
    } yield m)

  lazy val eof: Parser[Unit] = 
    Parser(for {
      s <- getS
      _ <- if (s.offset == s.string.length) Monad[M].unit else fail(Error("expected eof", s.offset))
    } yield ())

  // H elper algebras for working with the complicated monad
  private def liftE[A](inner: Inner[A]): M[A] =
    EitherT.liftF[Inner, Error, A](inner)

  private val getS: M[S] = liftE(StateT.get)

  private def fail[A](e: Error): M[A] = EitherT.left[Inner, Error, A](e)

  private def modify(f: S => S): M[S] =
    liftE(StateT.modify[Eval, S](f))

  private def advance(ct: Int): M[Unit] =
    modify(s => s.copy(offset = s.offset + ct)).void

  implicit val monadForParser: Monad[Parser] = new Monad[Parser] {
    def flatMap[A, B](fa: Parser[A])(g: A => Parser[B]): Parser[B] = fa.flatMap(g)
    def pure[A](a: A): Parser[A] = Parser(Monad[M].pure(a))
  }
}

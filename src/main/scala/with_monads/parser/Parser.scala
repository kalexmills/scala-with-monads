package with_monads.parser

import with_monads._
import with_monads.transformers._
import scala.util.matching.Regex

final class Parser[A](private val parser: Parser.M[A]) {
  def parse(s: String): Either[Error, A] = parser.value.runA(S(0, s)).value

  def map[B](f: A => B): Parser[B] = Parser(parser.map(f))

  def flatMap[B](f: A => Parser[B]): Parser[B] =
    Parser(parser.flatMap(a => f(a).parser))

  def or(other: Parser[A]): Parser[A] = 
    Parser(Parser.backtrack(this.parser).recoverWith(e => other.parser))
}

object Parser {
  type Inner[A] = StateT[Eval, S, A]
  type M[A] = EitherT[StateT[Eval, S, *], Error, A]

  def apply[A](parse: EitherT[StateT[Eval, S, *], Error, A]): Parser[A] =
    new Parser(parse)

  def always[A](a: A): Parser[A] = Parser(Monad[M].pure(a))

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

  def charWhere(f: Char => Boolean): Parser[Char] =
    Parser(for {
      s <- getS
      _ <- if (s.offset >= s.string.length)
        fail(Error(s"expect a char", s.offset))
      else Monad[M].unit
      c <- if (f(s.string.charAt(s.offset))) Monad[M].pure(s.string.charAt(s.offset))
      else fail(Error(s"expected a char", s.offset))
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
      m <- re
        .findPrefixMatchOf(s.string.drop(s.offset))
        .fold(fail[String](Error(s"expected $re", s.offset)))(
          m => Monad[M].pure(m.matched)
        )
      _ <- advance(m.length)
    } yield m)

  def delay[A](p: => Parser[A]): Parser[A] = 
    Parser(always(()).parser >> p.parser)

  def opt[A](p: Parser[A]): Parser[Option[A]] =
    Parser(
      backtrack(p.parser.map[Option[A]](Some(_)))
        .recoverWith(_ => Monad[M].pure(None))
    )

  def token[A](p: Parser[A]): Parser[A] = 
    p <* whitespace

  def many[A](p: Parser[A]): Parser[List[A]] = {
    def manyImpl(from: List[A]): Parser[List[A]] =
      opt(p).flatMap(_.fold(Monad[Parser].pure(from))(a => manyImpl(a :: from)))

    always(List.empty[A]).flatMap(manyImpl)
  }

  def many1[A](p: Parser[A]): Parser[List[A]] =
    p.flatMap(head => many(p).map(head :: _))


  lazy val digit: Parser[Char] = 
    charWhere(Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9').contains)


  lazy val int: Parser[Int] =
    for {
      sign <- opt(char('-'))
      text <- many1(digit)
    } yield (sign.fold("")(_.toString) ++ text).toInt

  lazy val eof: Parser[Unit] =
    Parser(for {
      s <- getS
      _ <- if (s.offset == s.string.length) Monad[M].unit
      else fail(Error("expected eof", s.offset))
    } yield ())

  lazy val whitespace: Parser[String] = regex("\\s*".r)

  // Helper algebras for working with the complicated monad
  private def liftE[A](inner: Inner[A]): M[A] =
    EitherT.liftF[Inner, Error, A](inner)

  private val getS: M[S] = liftE(StateT.get)

  private def setS(s: S): M[Unit] = liftE(StateT.set(s))

  private def fail[A](e: Error): M[A] = EitherT.left[Inner, Error, A](e)

  private def backtrack[A](ma: M[A]): M[A] =
    getS.flatMap(s => ma.recoverWith(e => setS(s) >> EitherT.left(e)))

  private def modify(f: S => S): M[S] =
    liftE(StateT.modify[Eval, S](f))

  private def advance(ct: Int): M[Unit] =
    modify(s => s.copy(offset = s.offset + ct)).void

  implicit val monadForParser: Monad[Parser] = new Monad[Parser] {
    def flatMap[A, B](fa: Parser[A])(g: A => Parser[B]): Parser[B] =
      fa.flatMap(g)
    def pure[A](a: A): Parser[A] = Parser(Monad[M].pure(a))
  }
}

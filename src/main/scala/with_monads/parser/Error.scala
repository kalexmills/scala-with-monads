package with_monads.parser

final case class Error(msg: String, pos: Int) extends Exception(s"parse error at $pos: $msg")
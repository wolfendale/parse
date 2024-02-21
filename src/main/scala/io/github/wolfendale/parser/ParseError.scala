package io.github.wolfendale.parser

final case class ParseError(error: String)

object ParseError:

  val UnexpectedEnd: ParseError = ParseError("Unexpected end of input")

end ParseError
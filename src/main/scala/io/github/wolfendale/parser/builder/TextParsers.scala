package io.github.wolfendale.parser.builder

import cats.Monad
import cats.syntax.all.*
import io.github.wolfendale.parser.{ParseError, ParseInput, ParserT}

transparent trait TextParsers[F[+_] : Monad]:

  val anyChar: ParserT[F, Char] =
    ParseInput.nextChar
      .orFail(ParseError.UnexpectedEnd)
    <~ ParseInput.advance(1)

  def take(number: Int): ParserT[F, String] =
    for
      string <- ParseInput.nextString(number)
        .ensure(ParseError.UnexpectedEnd)(_.length >= number)
      _      <- ParseInput.advance(number)
    yield string

  def takeWhile(p: Char => Boolean): ParserT[F, String] =
    for
      string <- ParseInput.takeWhile(p)
      _      <- ParseInput.advance(string.length)
    yield string

  def char(c: Char): ParserT[F, Char] =
    ParseInput.nextChar
      .orFail(ParseError.UnexpectedEnd)
      .ensureOr(c2 => ParseError(s"'$c2' was not '$c'"))(_ == c)
    <~ ParseInput.advance(1)

  def string(s: String): ParserT[F, String] =
    ParseInput.nextString(s.length)
      .ensureOr(s2 => ParseError(s"'$s2' was not '$s'"))(_ == s)
    <~ ParseInput.advance(s.length)

end TextParsers

package io.github.wolfendale.parser.builder

import cats.Monad
import cats.syntax.all.*
import io.github.wolfendale.parser.{ParseResult, ParserT}

transparent trait TextParsers[F[_] : Monad]:

  val anyChar: ParserT[F, Char] =
    state =>
      state.nextChar.map: c =>
        ParseResult.Completed(state.advance(1), c)
      .getOrElse(ParseResult.Failed(state, "no more input")).pure

  def char(c: Char): ParserT[F, Char] =
    state =>
      state.nextChar.map: c2 =>
        if c2 == c
        then ParseResult.Completed(state.advance(1), c)
        else ParseResult.Failed(state, s"'$c2' did not equal '$c'")
      .getOrElse(ParseResult.Failed(state, "no more input")).pure

  def string(s: String): ParserT[F, String] =
    state =>
      if state.nextString(s.length) == s
      then ParseResult.Completed(state.advance(s.length), s).pure
      else ParseResult.Failed(state, s"could not parse '$s'").pure

  def take(number: Int): ParserT[F, String] =
    state =>
      val string = state.nextString(number)
      if string.length == number
      then ParseResult.Completed(state.advance(number), string).pure
      else ParseResult.Failed(state, "no more input").pure

  def takeWhile(f: Char => Boolean): ParserT[F, String] =
    state =>
      val string = state.takeWhile(f)
      ParseResult.Completed(state.advance(string.length), string).pure

end TextParsers
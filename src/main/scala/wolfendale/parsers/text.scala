package wolfendale.parsers

import wolfendale.{ParseResult, Parser}

val anyChar: Parser[Char] =
  state =>
    state.nextChar.map: c =>
      ParseResult.Completed(state.advance(1), c)
    .getOrElse(ParseResult.Failed(state, "no more input"))

def char(c: Char): Parser[Char] =
  state =>
    state.nextChar.map: c2 =>
      if c2 == c
      then ParseResult.Completed(state.advance(1), c)
      else ParseResult.Failed(state, s"'$c2' did not equal '$c'")
    .getOrElse(ParseResult.Failed(state, "no more input"))

def string(s: String): Parser[String] =
  state =>
    if state.nextString(s.length) == s
    then ParseResult.Completed(state.advance(s.length), s)
    else ParseResult.Failed(state, s"could not parse '$s'")

def take(number: Int): Parser[String] =
  state =>
    val string = state.nextString(number)
    if string.length == number
    then ParseResult.Completed(state.advance(number), string)
    else ParseResult.Failed(state, "no more input")

def takeWhile(f: Char => Boolean): Parser[String] =
  state =>
    val string = state.takeWhile(f)
    ParseResult.Completed(state.advance(string.length), string)
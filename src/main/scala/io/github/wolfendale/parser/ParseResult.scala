package io.github.wolfendale.parser

final case class ParseResult[F[_], +A](state: ParseState[F], result: ParseError | A)

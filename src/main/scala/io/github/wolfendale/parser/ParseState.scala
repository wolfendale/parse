package io.github.wolfendale.parser

import cats.Monad

final case class ParseState[F[_]](input: ParseInput[F]):

  def withInput(input: ParseInput[F]): ParseState[F] =
    copy(input = input)
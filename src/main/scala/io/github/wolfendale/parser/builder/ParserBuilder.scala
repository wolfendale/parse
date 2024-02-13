package io.github.wolfendale.parser.builder

import cats.{Eval, Monad}
import io.github.wolfendale.parser.ParserT

final class ParserBuilder[F[_] : Monad] extends Combinators[F], TextParsers[F] {
  type Parser[A] = ParserT[F, A]
}

def of[F[_] : Monad]: ParserBuilder[F] = ParserBuilder[F]

val default: ParserBuilder[Eval] = of[Eval]

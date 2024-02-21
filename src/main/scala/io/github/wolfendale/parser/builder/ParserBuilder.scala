package io.github.wolfendale.parser.builder

import cats.{Eval, Monad}

final class ParserBuilder[F[+_] : Monad] extends Combinators[F], TextParsers[F]

def of[F[+_] : Monad]: ParserBuilder[F] = ParserBuilder[F]

val default: ParserBuilder[Eval] = of[Eval]

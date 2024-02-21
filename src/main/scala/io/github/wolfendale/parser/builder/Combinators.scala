package io.github.wolfendale.parser.builder

import cats.syntax.all.*
import cats.{Alternative, Foldable, Monad, NonEmptyAlternative}
import io.github.wolfendale.parser.{ParseError, ParseResult, ParserT}

transparent trait Combinators[F[+_] : Monad]:

  def success[A](result: A): ParserT[F, A] =
    ParserT.success(result)

  def failed[B](error: ParseError): ParserT[F, B] =
    ParserT.failed(error)

  def lookahead[A](parser: ParserT[F, A]): ParserT[F, A] =
    for
      state <- ParserT.getState
      a     <- parser
      _     <- ParserT.replace(state)
    yield a

  def matches(parser: ParserT[F, Any]): ParserT[F, Boolean] =
    lookahead(parser).as(true) | success(false)

  def zeroOrMore[A](parser: ParserT[F, A]): ParserT[F, Seq[A]] =
    orMore(Seq.empty, parser)

  def zeroOrMore[G[_]]: PartiallyAppliedZeroOrMore[F, G] =
    PartiallyAppliedZeroOrMore[F, G]()

  def oneOrMore[A](parser: ParserT[F, A]): ParserT[F, Seq[A]] =
    parser.flatMap(a => orMore(Seq(a), parser))

  def oneOrMore[G[_]]: PartiallyAppliedOneOrMore[F, G] =
    PartiallyAppliedOneOrMore[F, G]()

  def end: ParserT[F, Unit] =
    ParserT: state =>
      state.input.remaining.map:
        _.map: remaining =>
          if remaining == 0
          then ParseResult(state, ())
          else ParseResult(state, ParseError("input remaining"))
        .getOrElse(ParseResult(state, ParseError("indeterminate input left")))

  def lazily[A](parser: => ParserT[F, A]): ParserT[F, A] =
    lazy val cached: ParserT[F, A] = parser
    ParserT(cached.parse)


  def repeat[A](parser: ParserT[F, A], times: Int): ParserT[F, Seq[A]] =
    repeatImpl[F, Seq, A](parser, times)

  def repeat[G[_]]: PartiallyAppliedRepeat[F, G] =
    PartiallyAppliedRepeat[F, G]()

end Combinators

// TODO is there a different pattern for this in Scala 3?
private final class PartiallyAppliedZeroOrMore[F[+_], G[_]](private val dummy: Boolean = true) extends AnyVal:
  def apply[A](parser: ParserT[F, A], dummy: Unit = ())(using Monad[F])(using G: Alternative[G]): ParserT[F, G[A]] =
    orMore(G.empty, parser)

// TODO is there a different pattern for this in Scala 3?
private final class PartiallyAppliedOneOrMore[F[+_], G[_]](private val dummy: Boolean = true) extends AnyVal:
  def apply[A](parser: ParserT[F, A], dummy: Unit = ())(using Monad[F])(using G: NonEmptyAlternative[G]): ParserT[F, G[A]] =
    parser.flatMap(a => orMore(G.pure(a), parser))

private def orMore[F[+_] : Monad, G[_] : NonEmptyAlternative, A](initial: G[A], parser: ParserT[F, A]): ParserT[F, G[A]] =
  Monad[[X] =>> ParserT[F, X]].tailRecM(initial): ga =>
    parser.map(a => Left(ga.appendK(a))) | ParserT.success[F, Either[G[A], G[A]]](Right(ga))

// TODO is there a different pattern for this in Scala 3?
private class PartiallyAppliedRepeat[F[+_], G[_]](private val dummy: Boolean = true) extends AnyVal:
  def apply[A](parser: ParserT[F, A], times: Int, dummy: Unit = ())(using Monad[F])(using G: Alternative[G])(using Foldable[G]): ParserT[F, G[A]] =
    repeatImpl(parser, times)

private def repeatImpl[F[+_] : Monad, G[_], A](parser: ParserT[F, A], times: Int)(using G: Alternative[G])(using Foldable[G]): ParserT[F, G[A]] =
  Monad[[X] =>> ParserT[F, X]].tailRecM(G.empty[A]): ga =>
    val count = ga.foldl(0)((m, _) => m + 1)
    if count < times
    then parser.map(a => Left(ga.appendK(a)))
    else ParserT.success[F, Either[G[A], G[A]]](Right(ga))
  | ParserT.failed(ParseError.UnexpectedEnd)
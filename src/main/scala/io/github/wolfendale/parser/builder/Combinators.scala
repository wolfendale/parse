package io.github.wolfendale.parser.builder

import cats.{Alternative, Foldable, Monad, NonEmptyAlternative}
import cats.syntax.all.*
import io.github.wolfendale.parser.{ParseResult, ParserT}

import scala.util.Right

transparent trait Combinators[F[_] : Monad]:

  final def success[A](result: A): ParserT[F, A] =
    ParserT.success(result)

  final def failed(error: String): ParserT[F, Nothing] =
    ParserT.failed[F](error)

  def lookahead[A](parser: ParserT[F, A]): ParserT[F, A] =
    state =>
      parser.parse(state).map:
        case ParseResult.Completed(_, result) =>
          ParseResult.Completed(state, result)
        case failed: ParseResult.Failed =>
          failed

  def matches(parser: ParserT[F, _]): ParserT[F, Boolean] =
    lookahead(parser).map(_ => true) | success(false)

  def zeroOrMore[A](parser: ParserT[F, A]): ParserT[F, Seq[A]] =
    orMore(Seq.empty, parser)

  def zeroOrMore[G[_]]: PartiallyAppliedZeroOrMore[F, G] =
    PartiallyAppliedZeroOrMore[F, G]()

  def oneOrMore[A](parser: ParserT[F, A]): ParserT[F, Seq[A]] =
    parser.flatMap(a => orMore(Seq(a), parser))

  def oneOrMore[G[_]]: PartiallyAppliedOneOrMore[F, G] =
    PartiallyAppliedOneOrMore[F, G]()

  val end: ParserT[F, Unit] =
    state =>
      state.remaining.map: remaining =>
        if remaining == 0
        then ParseResult.Completed(state, ())
        else ParseResult.Failed(state, "input remaining")
      .getOrElse(???).pure // TODO

  def lazily[A](parser: => ParserT[F, A]): ParserT[F, A] =
    lazy val cachedParser = parser
    cachedParser.parse(_)

  val position: ParserT[F, Int] =
    state =>
      ParseResult.Completed(state, state.position).pure

  def repeat[A](parser: ParserT[F, A], times: Int): ParserT[F, Seq[A]] =
    repeatImpl(parser, times)

  def repeat[G[_]]: PartiallyAppliedRepeat[F, G] =
    PartiallyAppliedRepeat[F, G]()

end Combinators

// TODO is there a different pattern for this in Scala 3?
private final class PartiallyAppliedZeroOrMore[F[_], G[_]](private val dummy: Boolean = true) extends AnyVal:
  def apply[A](parser: ParserT[F, A], dummy: Unit = ())(using Monad[F])(using G: Alternative[G]): ParserT[F, G[A]] =
    orMore(G.empty, parser)

// TODO is there a different pattern for this in Scala 3?
private final class PartiallyAppliedOneOrMore[F[_], G[_]](private val dummy: Boolean = true) extends AnyVal:
  def apply[A](parser: ParserT[F, A], dummy: Unit = ())(using Monad[F])(using G: NonEmptyAlternative[G]): ParserT[F, G[A]] =
    parser.flatMap(a => orMore(G.pure(a), parser))

// TODO is there a different pattern for this in Scala 3?
private class PartiallyAppliedRepeat[F[_], G[_]](private val dummy: Boolean = true) extends AnyVal:
  def apply[A](parser: ParserT[F, A], times: Int, dummy: Unit = ())(using Monad[F])(using G: Alternative[G])(using Foldable[G]): ParserT[F, G[A]] =
    repeatImpl(parser, times)

private def orMore[F[_] : Monad, A, G[_] : NonEmptyAlternative](initial: G[A], parser: ParserT[F, A]): ParserT[F, G[A]] =
  Monad[[X] =>> ParserT[F, X]].tailRecM(initial): ga =>
    parser.map(a => Left(ga.appendK(a))) | ParserT.success(Right(ga))
    
// TODO think about removing the Foldable, have a different way of tracking number of times
private def repeatImpl[F[_] : Monad, G[_], A](parser: ParserT[F, A], times: Int)(using G: Alternative[G])(using Foldable[G]): ParserT[F, G[A]] =
  Monad[[X] =>> ParserT[F, X]].tailRecM(G.empty[A]): ga =>
    val count = ga.foldl(0)((m, _) => m + 1)
    if count < times
    then parser.map(a => Left(ga.appendK(a)))
    else ParserT.success(Right(ga))
  | ParserT.failed("no more input")
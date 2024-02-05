package wolfendale.parsers

import cats.{Alternative, Foldable, Monad, NonEmptyAlternative}
import cats.syntax.all.*
import wolfendale.{ParseResult, ParseContext, Parser}

def success[A](result: A): Parser[A] =
  ParseResult.Completed(_, result)

def failed(error: String): Parser[Nothing] =
  ParseResult.Failed(_, error)

def lookahead[A](parser: Parser[A]): Parser[A] =
  state =>
    parser.parse(state) match
      case ParseResult.Completed(_, result) =>
        ParseResult.Completed(state, result)
      case f: ParseResult.Failed =>
        f

def matches(parser: Parser[_]): Parser[Boolean] =
  lookahead(parser).map(_ => true) | success(false)

def zeroOrMore[A](parser: Parser[A])
                 (using c: ParseContext)
                 (using G: Alternative[c.CollectionType]): Parser[c.CollectionType[A]] =
  orMore(G.empty, parser)

def zeroOrMore[G[_]]: PartiallyAppliedZeroOrMore[G] =
  PartiallyAppliedZeroOrMore[G]()

// TODO is there a different pattern for this in Scala 3?
private final class PartiallyAppliedZeroOrMore[G[_]](private val dummy: Boolean = true) extends AnyVal:
  // This dummy parameter helps the compiler choose which instance of `zeroOrMore` to pick
  def apply[A](parser: Parser[A], dummy: Unit = ())(using G: Alternative[G]): Parser[G[A]] =
    orMore(G.empty, parser)

def oneOrMore[A](parser: Parser[A])
                (using settings: ParseContext)
                (using G: NonEmptyAlternative[settings.NonEmptyCollectionType]): Parser[settings.NonEmptyCollectionType[A]] =
  parser.flatMap(a => orMore(G.pure(a), parser))

def oneOrMore[G[_]]: PartiallyAppliedOneOrMore[G] =
  PartiallyAppliedOneOrMore[G]()

// TODO is there a different pattern for this in Scala 3?
private final class PartiallyAppliedOneOrMore[G[_]](private val dummy: Boolean = true) extends AnyVal:
  // This dummy parameter helps the compiler choose which instance of `oneOrMore` to pick
  def apply[A](parser: Parser[A], dummy: Unit = ())(using G: NonEmptyAlternative[G]): Parser[G[A]] =
    parser.flatMap(a => orMore(G.pure(a), parser))

private def orMore[A, G[_]](initial: G[A], parser: Parser[A])(using G: NonEmptyAlternative[G]): Parser[G[A]] =
  Monad[Parser].tailRecM(initial): ga =>
    parser.map(a => Left(ga.appendK(a))) | success(Right(ga))

val end: Parser[Unit] =
  state =>
    state.remaining.map: remaining =>
      if remaining == 0
      then ParseResult.Completed(state, ())
      else ParseResult.Failed(state, "input remaining")
    .getOrElse(???) // TODO

def lazily[A](parser: => Parser[A]): Parser[A] =
  lazy val cachedParser = parser
  cachedParser.parse(_)

val position: Parser[Int] =
  state =>
    ParseResult.Completed(state, state.position)

def repeat[A](parser: Parser[A], times: Int)
             (using c: ParseContext)
             (using G: Alternative[c.CollectionType])
             (using Foldable[c.CollectionType]): Parser[c.CollectionType[A]] =
  repeatImpl(parser, times)

def repeat[G[_]]: PartiallyAppliedRepeat[G] =
  PartiallyAppliedRepeat[G]()

private class PartiallyAppliedRepeat[G[_]](private val dummy: Boolean = true) extends AnyVal:
  // This dummy parameter helps the compiler choose which instance of `repeat` to pick
  def apply[A](parser: Parser[A], times: Int, dummy: Unit = ())(using G: Alternative[G])(using Foldable[G]): Parser[G[A]] =
    repeatImpl(parser, times)

// TODO think about removing the Foldable, have a different way of tracking number of times
private def repeatImpl[G[_], A](parser: Parser[A], times: Int)(using G: Alternative[G])(using Foldable[G]): Parser[G[A]] =
  Monad[Parser].tailRecM(G.empty[A]): ga =>
    val count = ga.foldl(0)((m, _) => m + 1)
    if count < times
    then parser.map(a => Left(ga.appendK(a)))
    else success(Right(ga))
  | failed("no more input")

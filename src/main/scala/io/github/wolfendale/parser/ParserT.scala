package io.github.wolfendale.parser

import cats.*
import cats.syntax.all.*

import scala.annotation.targetName

abstract class ParserT[F[_], A]:

  def parse(state: ParseState): F[ParseResult[A]]

  final def map[B](f: A => B)(using Functor[F]): ParserT[F, B] =
    parse(_).map(_.map(f))

  final def as[B](b: B)(using Functor[F]): ParserT[F, B] =
    map(_ => b)

  final def flatMap[B](f: A => ParserT[F, B])(using Monad[F]): ParserT[F, B] =
    parse(_).flatMap:
      case ParseResult.Completed(state, result) =>
        f(result).parse(state)
      case failed: ParseResult.Failed =>
        failed.pure[F]

  @targetName("and")
  final def ~[B](other: ParserT[F, B])(using Monad[F]): ParserT[F, (A, B)] =
    for a <- this; b <- other yield (a, b)

  @targetName("keepRight")
  final def ~>[B](other: ParserT[F, B])(using Monad[F]): ParserT[F, B] =
    for _ <- this; b <- other yield b

  @targetName("keepLeft")
  final def <~(other: ParserT[F, _])(using Monad[F]): ParserT[F, A] =
    for a <- this; _ <- other yield a

  @targetName("or")
  final def |[B](other: ParserT[F, B])(using Monad[F]): ParserT[F, A | B] =
    state =>
      parse(state).flatMap:
        case completed: ParseResult.Completed[A] =>
          completed.pure
        case _ =>
          other.parse(state).widen

end ParserT

object ParserT:
  
  def success[F[_] : Applicative, A](a: A): ParserT[F, A] =
    ParseResult.Completed(_, a).pure
    
  def failed[F[_]: Applicative](error: String): ParserT[F, Nothing] =
    ParseResult.Failed(_, error).pure

  given [F[_] : Monad]: StackSafeMonad[[A] =>> ParserT[F, A]] with

    override def pure[A](x: A): ParserT[F, A] =
      ParseResult.Completed(_, x).pure

    override def flatMap[A, B](fa: ParserT[F, A])(f: A => ParserT[F, B]): ParserT[F, B] =
      fa.flatMap(f)

  end given

end ParserT

package io.github.wolfendale.parser

import cats.syntax.all.*
import cats.{Monad, MonadError, StackSafeMonad}

import scala.annotation.targetName

final case class ParserT[F[+_] : Monad, +A](run: ParseState[F] => F[ParseResult[F, A]]):

  def parse(state: ParseState[F]): F[ParseResult[F, A]] =
    run(state)

  def parse(state: String): F[ParseResult[F, A]] =
    parse(ParseState(StringParseInput(state)))

  def flatMap[B](f: A => ParserT[F, B]): ParserT[F, B] =
    ParserT[F, B]: state =>
      parse(state).flatMap: result =>
        result.result match
          case error: ParseError =>
            result.asInstanceOf[ParseResult[F, B]].pure
          case a =>
            // TODO is there a way to prove to the compiler this is safe?
            f(a.asInstanceOf[A]).parse(result.state)

  def handleErrorWith[B >: A](f: ParseError => ParserT[F, B]): ParserT[F, B] =
    ParserT: state =>
      parse(state).flatMap: result =>
        result.result match
          case error: ParseError =>
            f(error).parse(result.state.withInput(state.input))
          case _ =>
            result.pure

  @targetName("and")
  def ~[B](other: ParserT[F, B]): ParserT[F, (A, B)] =
    for a <- this; b <- other yield (a, b)

  @targetName("keepRight")
  def ~>[B](other: ParserT[F, B]): ParserT[F, B] =
    for _ <- this; b <- other yield b

  @targetName("andThen")
  def ~>>[B](f: A => ParserT[F, B]): ParserT[F, B] =
    for a <- this; b <- f(a) yield b

  @targetName("keepLeft")
  def <~[B](other: ParserT[F, B]): ParserT[F, A] =
    for a <- this; _ <- other yield a

  @targetName("when")
  def <<~[B](f: A => ParserT[F, B]): ParserT[F, A] =
    for a <- this; _ <- f(a) yield a

  @targetName("or")
  def |[B](other: ParserT[F, B]): ParserT[F, A | B] =
    handleErrorWith(_ => other)

end ParserT

object ParserT:

  def getState[F[+_] : Monad]: ParserT[F, ParseState[F]] =
    ParserT(state => ParseResult(state, state).pure)
    
  def inspect[F[+_] : Monad, A](run: ParseState[F] => F[ParseError | A]): ParserT[F, A] =
    ParserT(state => run(state).map(ParseResult(state, _)))

  def replace[F[+_] : Monad](state: ParseState[F]): ParserT[F, Unit] =
    ParserT(_ => ParseResult(state, ()).pure)

  def modify[F[+_] : Monad](run: ParseState[F] => F[ParseState[F]]): ParserT[F, Unit] =
    ParserT(state => run(state).map(ParseResult(_, ())))

  def success[F[+_] : Monad, A](a: A): ParserT[F, A] =
    ParserT(ParseResult(_, a).pure)

  def successF[F[+_] : Monad, A](fa: F[A]): ParserT[F, A] =
    ParserT(state => fa.map(ParseResult(state, _)))

  def failed[F[+_] : Monad](error: ParseError): ParserT[F, Nothing] =
    ParserT(ParseResult(_, error).pure)

  given [F[+_] : Monad]: MonadError[[X] =>> ParserT[F, X], ParseError]
    with StackSafeMonad[[X] =>> ParserT[F, X]] with

      override def flatMap[A, B](fa: ParserT[F, A])(f: A => ParserT[F, B]): ParserT[F, B] =
        fa.flatMap(f)

      override def pure[A](x: A): ParserT[F, A] =
        success(x)

      override def raiseError[A](e: ParseError): ParserT[F, A] =
        failed(e)

      override def handleErrorWith[A](fa: ParserT[F, A])(f: ParseError => ParserT[F, A]): ParserT[F, A] =
        fa.handleErrorWith(f)

  extension [F[+_] : Monad, A](parser: ParserT[F, Option[A]])

    def orFail(error: ParseError): ParserT[F, A] =
      parser.flatMap: oa =>
        oa.map(ParserT.success(_))
          .getOrElse(ParserT.failed[F](error))

  end extension

end ParserT




package io.github.wolfendale.parser

import cats.Monad
import cats.syntax.all.*

trait ParseInput[F[_]]:

  def position: F[Int]
  def advance(count: Int): F[ParseInput[F]]
  def nextChar: F[Option[Char]]
  def nextString(length: Int): F[String]
  def takeWhile(p: Char => Boolean): F[String]
  def remaining: F[Option[Int]]

end ParseInput

object ParseInput:

  def get[F[+_] : Monad]: ParserT[F, ParseInput[F]] =
    ParserT.inspect[F, ParseInput[F]](_.input.pure)

  def position[F[+_] : Monad]: ParserT[F, Int] =
    get.flatMap(input => ParserT.successF(input.position))

  def advance[F[+_] : Monad](count: Int): ParserT[F, Unit] =
    ParserT.modify: state =>
      state.input.advance(count).map(input => state.copy(input = input))

  def nextChar[F[+_] : Monad]: ParserT[F, Option[Char]] =
    get.flatMap(input => ParserT.successF(input.nextChar))

  def nextString[F[+_] : Monad](length: Int): ParserT[F, String] =
    get.flatMap(input => ParserT.successF(input.nextString(length)))

  def takeWhile[F[+_] : Monad](p: Char => Boolean): ParserT[F, String] =
    get.flatMap(input => ParserT.successF(input.takeWhile(p)))

end ParseInput
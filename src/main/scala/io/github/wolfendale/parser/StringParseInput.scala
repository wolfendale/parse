package io.github.wolfendale.parser

import cats.syntax.all.*
import cats.Monad

final case class StringParseInput[F[_] : Monad](input: String, _position: Int = 0) extends ParseInput[F]:

  require(_position > -1)

  override def position: F[Int] =
    _position.pure

  override def advance(count: Int): F[ParseInput[F]] =
    copy(_position = _position + count).pure

  override def nextChar: F[Option[Char]] =
    input.lift(_position).pure

  override def nextString(length: Int): F[String] =
    input.substring(_position, (_position + length) min input.length).pure

  override def takeWhile(p: Char => Boolean): F[String] =
    input.substring(_position, input.length).takeWhile(p).pure

  override def remaining: F[Option[Int]] =
    Some(input.length - _position).pure

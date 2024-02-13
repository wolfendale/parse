package io.github.wolfendale.parser

trait ParseState:
  def position: Int
  def nextChar: Option[Char]
  def nextString(length: Int): String
  def takeWhile(f: Char => Boolean): String
  def advance(n: Int): ParseState
  def remaining: Option[Int]

object ParseState:

  given Conversion[String, ParseState] =
    StringParseState(_, 0)

final case class StringParseState(input: String, position: Int) extends ParseState:

  require(position >= 0)

  override def nextChar: Option[Char] =
    input.lift(position)

  override def nextString(length: Int): String =
    input.substring(position, (position + length) min input.length)
    
  override def takeWhile(f: Char => Boolean): String =
    input.takeWhile(f)

  override def advance(n: Int): ParseState =
    copy(position = position + n)

  override def remaining: Option[Int] =
    Some(input.length - position)

end StringParseState

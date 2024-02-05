package wolfendale

import cats.*
import wolfendale.parsers.success

import scala.annotation.targetName

abstract class Parser[A]:

  def parse(state: ParseState): ParseResult[A]

  final def map[B](f: A => B): Parser[B] =
    parse(_).map(f)
      
  final def as[B](b: B): Parser[B] =
    map(_ => b)

  final def flatMap[B](f: A => Parser[B]): Parser[B] =
    parse(_) match
      case ParseResult.Completed(state, result) =>
        f(result).parse(state)
      case failed: ParseResult.Failed =>
        failed

  @targetName("and")
  final def ~[B](other: Parser[B]): Parser[(A, B)] =
    for a <- this; b <- other yield (a, b)

  @targetName("keepRight")
  final def ~>[B](other: Parser[B]): Parser[B] =
    for _ <- this; b <- other yield b

  @targetName("keepLeft")
  final def <~(other: Parser[_]): Parser[A] =
    for a <- this; _ <- other yield a

  @targetName("or")
  final def |[B](other: Parser[B]): Parser[A | B] =
    state =>
      parse(state) match
        case c: ParseResult.Completed[A] =>
          c
        case f: ParseResult.Failed =>
          other.parse(state)

end Parser

object Parser:

  given StackSafeMonad[Parser] with // TODO this probably isn't stack safe oops
    
    override def pure[A](x: A): Parser[A] =
      success(x)

    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
      fa.flatMap(f)
    
  end given
      
end Parser

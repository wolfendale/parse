package wolfendale

import cats.*

enum ParseResult[+A]:

  case Completed(state: ParseState, result: A) extends ParseResult[A]
  case Failed(state: ParseState, error: String) extends ParseResult[Nothing]

  def map[B](f: A => B): ParseResult[B] =
    this match
      case Completed(state, result) => Completed(state, f(result))
      case f: Failed => f

end ParseResult

object ParseResult:

  given Functor[ParseResult] with
    override def map[A, B](fa: ParseResult[A])(f: A => B): ParseResult[B] =
      fa.map(f)

end ParseResult

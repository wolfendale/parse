package io.github.wolfendale.parser

import cats.Eval
import cats.syntax.all.*
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers

import scala.reflect.ClassTag

transparent trait ParseResultExtensions extends EitherValues:
  self: Matchers =>

  extension [A : ClassTag](result: ParseResult[Eval, A])

    def asCompleted: A =
      result.result mustBe an[A]
      result.result.asInstanceOf[A]

    def asFailed: ParseError =
      result.result mustBe a[ParseError]
      result.result.asInstanceOf[ParseError]
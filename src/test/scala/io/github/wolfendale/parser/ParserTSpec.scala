package io.github.wolfendale.parser

import cats._
import cats.instances.all.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ParserTSpec extends AnyFreeSpec, Matchers, ParseResultExtensions:
  
  import builder.default.*
  
  "~" - {

    val first = char('a')
    val second = char('b')
    val parser = first ~ second

    "must produce the result of both parsers when they are successful" in:
      val result = parser.parse("ab").value.asCompleted
      result.result mustEqual ('a', 'b')
      result.state.position mustEqual 2

    "must fail with the result of the first parser if it fails" in:
      val input = "b"
      val result = parser.parse(input).value
      val firstResult = first.parse(input).value

      result mustBe a[ParseResult.Failed]
      result mustEqual firstResult

    "must fail with the result of the second parser is it fails" in:
      val input = "a"
      val result = parser.parse(input).value
      val secondResult = second.parse(StringParseState(input, 1)).value

      result mustBe a[ParseResult.Failed]
      result mustEqual secondResult
  }

  "~>" - {

    val first = char('a')
    val second = char('b')
    val parser = first ~> second

    "must produce the result of the second parser when they are both successful" in:
      val result = parser.parse("ab").value.asCompleted
      result.result mustEqual('b')
      result.state.position mustEqual 2

    "must fail with the result of the first parser if it fails" in:
      val input = "b"
      val result = parser.parse(input).value
      val firstResult = first.parse(input).value

      result mustBe a[ParseResult.Failed]
      result mustEqual firstResult

    "must fail with the result of the second parser is it fails" in:
      val input = "a"
      val result = parser.parse(input).value
      val secondResult = second.parse(StringParseState(input, 1)).value

      result mustBe a[ParseResult.Failed]
      result mustEqual secondResult
  }

  "<~" - {

    val first = char('a')
    val second = char('b')
    val parser = first <~ second

    "must produce the result of the first parser when they are successful both" in:
      val result = parser.parse("ab").value.asCompleted
      result.result mustEqual ('a')
      result.state.position mustEqual 2

    "must fail with the result of the first parser if it fails" in:
      val input = "b"
      val result = parser.parse(input).value
      val firstResult = first.parse(input).value

      result mustBe a[ParseResult.Failed]
      result mustEqual firstResult

    "must fail with the result of the second parser is it fails" in:
      val input = "a"
      val result = parser.parse(input).value
      val secondResult = second.parse(StringParseState(input, 1)).value

      result mustBe a[ParseResult.Failed]
      result mustEqual secondResult
  }

  "|" - {

    "must produce the result of the first parser if it's successful" in:
      val parser = success(1) | success(2)
      val result = parser.parse("").value.asCompleted
      result.result mustEqual 1

    "must produce the result of the second parser if the first fails but the second doesn't" in:
      val parser = failed("error") | success(2)
      val result = parser.parse("").value.asCompleted
      result.result mustEqual 2

    "must return the error from the last failing parser" in:
      val parser = failed("error1") | failed("error2")
      val result = parser.parse("").value.asFailed
      result.error mustEqual "error2"
  }

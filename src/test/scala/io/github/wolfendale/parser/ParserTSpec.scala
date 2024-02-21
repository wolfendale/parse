package io.github.wolfendale.parser

import cats.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class ParserTSpec extends AnyFreeSpec, Matchers, ParseResultExtensions:

  import builder.default.*

  "~" - {

    val first = char('a')
    val second = char('b')
    val parser = first ~ second

    "must produce the result of both parsers when they are successful" in:
      val result = parser.parse("ab").value
      result.result mustEqual ('a', 'b')
      result.state.input.position.value mustEqual 2

    "must fail with the result of the first parser if it fails" in:
      val input = "b"
      val result = parser.parse(input).value
      val firstResult = first.parse(input).value

      result.result mustBe a[ParseError]
      result mustEqual firstResult

    "must fail with the result of the second parser is it fails" in:
      val input = "a"
      val result = parser.parse(input).value
      val secondResult = second.parse(ParseState[Eval](StringParseInput(input, 1))).value

      result.result mustBe a[ParseError]
      result mustEqual secondResult
  }

  "~>" - {

    val first = char('a')
    val second = char('b')
    val parser = first ~> second

    "must produce the result of the second parser when they are both successful" in:
      val result = parser.parse("ab").value
      result.result mustEqual 'b'
      result.state.input.position.value mustEqual 2

    "must fail with the result of the first parser if it fails" in:
      val input = "b"
      val result = parser.parse(input).value
      val firstResult = first.parse(input).value

      result.result mustBe a[ParseError]
      result mustEqual firstResult

    "must fail with the result of the second parser is it fails" in:
      val input = "a"
      val result = parser.parse(input).value
      val secondResult = second.parse(ParseState[Eval](StringParseInput(input, 1))).value

      result.result mustBe a[ParseError]
      result mustEqual secondResult
  }

  "<~" - {

    val first = char('a')
    val second = char('b')
    val parser = first <~ second

    "must produce the result of the first parser when they are successful both" in:
      val result = parser.parse("ab").value
      result.result mustEqual 'a'
      result.state.input.position.value mustEqual 2

    "must fail with the result of the first parser if it fails" in:
      val input = "b"
      val result = parser.parse(input).value
      val firstResult = first.parse(input).value

      result.result mustBe a[ParseError]
      result mustEqual firstResult

    "must fail with the result of the second parser is it fails" in:
      val input = "a"
      val result = parser.parse(input).value
      val secondResult = second.parse(ParseState[Eval](StringParseInput(input, 1))).value

      result.result mustBe a[ParseError]
      result mustEqual secondResult
  }

  "|" - {

    "must produce the result of the first parser if it's successful" in:
      val parser = success(1) | success(2)
      val result = parser.parse("").value.asCompleted
      result mustEqual 1

    "must produce the result of the second parser if the first fails but the second doesn't" in:
      val parser = failed(ParseError("error")) | success(2)
      val result = parser.parse("").value.asCompleted
      result mustEqual 2

    "must return the error from the last failing parser" in:
      val parser: ParserT[Eval, Unit] = failed(ParseError("error1")) | failed(ParseError("error2"))
      val result = parser.parse("").value.asFailed
      result mustEqual ParseError("error2")
  }

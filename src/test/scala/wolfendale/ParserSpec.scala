package wolfendale

import cats.Eval
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import wolfendale.parsers.*

class ParserSpec extends AnyFreeSpec, Matchers, ParseResultExtensions:

  "~" - {

    val first = char('a')
    val second = char('b')
    val parser = first ~ second

    "must produce the result of both parsers when they are successful" in:
      val result = parser.parse("ab").asCompleted
      result.result mustEqual ('a', 'b')
      result.state.position mustEqual 2

    "must fail with the result of the first parser if it fails" in:
      val input = "b"
      val result = parser.parse(input)
      val firstResult = first.parse(input)

      result mustBe a[ParseResult.Failed]
      result mustEqual firstResult

    "must fail with the result of the second parser is it fails" in:
      val input = "a"
      val result = parser.parse(input)
      val secondResult = second.parse(StringParseState(input, 1))

      result mustBe a[ParseResult.Failed]
      result mustEqual secondResult
  }

  "~>" - {

    val first = char('a')
    val second = char('b')
    val parser = first ~> second

    "must produce the result of the second parser when they are both successful" in:
      val result = parser.parse("ab").asCompleted
      result.result mustEqual('b')
      result.state.position mustEqual 2

    "must fail with the result of the first parser if it fails" in:
      val input = "b"
      val result = parser.parse(input)
      val firstResult = first.parse(input)

      result mustBe a[ParseResult.Failed]
      result mustEqual firstResult

    "must fail with the result of the second parser is it fails" in:
      val input = "a"
      val result = parser.parse(input)
      val secondResult = second.parse(StringParseState(input, 1))

      result mustBe a[ParseResult.Failed]
      result mustEqual secondResult
  }

  "<~" - {

    val first = char('a')
    val second = char('b')
    val parser = first <~ second

    "must produce the result of the first parser when they are successful both" in:
      val result = parser.parse("ab").asCompleted
      result.result mustEqual ('a')
      result.state.position mustEqual 2

    "must fail with the result of the first parser if it fails" in:
      val input = "b"
      val result = parser.parse(input)
      val firstResult = first.parse(input)

      result mustBe a[ParseResult.Failed]
      result mustEqual firstResult

    "must fail with the result of the second parser is it fails" in:
      val input = "a"
      val result = parser.parse(input)
      val secondResult = second.parse(StringParseState(input, 1))

      result mustBe a[ParseResult.Failed]
      result mustEqual secondResult
  }

  "|" - {

    "must produce the result of the first parser if it's successful" in:
      val parser = success(1) | success(2)
      val result = parser.parse("").asCompleted
      result.result mustEqual 1

    "must produce the result of the second parser if the first fails but the second doesn't" in:
      val parser = failed("error") | success(2)
      val result = parser.parse("").asCompleted
      result.result mustEqual 2

    "must return the error from the last failing parser" in:
      val parser = failed("error1") | failed("error2")
      val result = parser.parse("").asFailed
      result.error mustEqual "error2"
  }

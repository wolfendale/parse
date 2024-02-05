package wolfendale.parsers

import org.scalacheck.{Gen, Shrink}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import wolfendale.{ParseResultExtensions, StringParseState}

class TextParserSpec extends AnyFreeSpec, Matchers, ScalaCheckDrivenPropertyChecks, ParseResultExtensions:

  given [A]: Shrink[A] =
    Shrink.shrinkAny

  "anyChar" - {

    "must parse any char in the input" in:

      val inputGen = for
        length   <- Gen.chooseNum(1, 100)
        input    <- Gen.stringOfN(length, Gen.alphaNumChar)
        position <- Gen.chooseNum(0, input.length - 1)
      yield (input, position)

      forAll(inputGen): (input, position) =>
        val result = anyChar.parse(StringParseState(input, position)).asCompleted
        result.result mustEqual input(position)
        result.state.position mustEqual position + 1

    "must fail if there is no more input" in:

      val inputGen = for
        length   <- Gen.chooseNum(0, 100)
        input    <- Gen.stringOfN(length, Gen.alphaNumChar)
        position <- Gen.chooseNum(length, length + 100)
      yield (input, position)

      forAll(inputGen): (input, position) =>
        val result = anyChar.parse(StringParseState(input, position)).asFailed
        result.state.position mustEqual position
        result.error mustEqual "no more input"
  }

  "char" - {

    "must parse a char if it is the next char to be parsed in the input" in:

      val inputGen = for
        length   <- Gen.chooseNum(1, 100)
        input    <- Gen.stringOfN(length, Gen.alphaNumChar)
        position <- Gen.chooseNum(0, length - 1)
      yield (input, position)

      forAll(inputGen): (input, position) =>
        val expected = input(position)
        val parser = char(expected)
        val result = parser.parse(StringParseState(input, position)).asCompleted
        result.result mustEqual expected
        result.state.position mustEqual position + 1

    "must fail if the given char is not the next char to be parsed in the input" in:

      val inputGen = for
        length   <- Gen.chooseNum(1, 100)
        input    <- Gen.stringOfN(length, Gen.alphaNumChar)
        position <- Gen.chooseNum(0, length - 1)
        expected <- Gen.alphaNumChar.suchThat(_ != input(position))
      yield (input, position, expected)

      forAll(inputGen): (input, position, expected) =>
        val parser = char(expected)
        val result = parser.parse(StringParseState(input, position)).asFailed
        result.state.position mustEqual position
        result.error mustEqual s"'${input(position)}' did not equal '$expected'"

    "must fail if there is no more input to be consumed" in:

      val inputGen = for
        length   <- Gen.chooseNum(0, 100)
        input    <- Gen.stringOfN(length, Gen.alphaNumChar)
        position <- Gen.chooseNum(input.length, input.length + 100)
        c        <- Gen.alphaNumChar
      yield (input, position, c)

      forAll(inputGen): (input, position, c) =>
        val parser = char(c)
        val result = parser.parse(StringParseState(input, position)).asFailed
        result.state.position mustEqual position
        result.error mustEqual "no more input"
  }

  "string" - {

    "must parse a string if the input matches it" in:

      val inputGen = for
        expected <- Gen.alphaNumStr
        before   <- Gen.alphaNumStr
        after    <- Gen.alphaNumStr
      yield (before + expected + after, before.length, expected)

      forAll(inputGen): (input, position, expected) =>
        val parser = string(expected)
        val result = parser.parse(StringParseState(input, position)).asCompleted
        result.result mustEqual expected
        result.state.position mustEqual position + expected.length

    "must fail if the expected string is not next in the input" in:

      val inputGen = for
        expected <- Gen.alphaNumStr
        input    <- Gen.alphaNumStr.suchThat(!_.startsWith(expected))
      yield (input, expected)

      forAll(inputGen): (input, expected) =>
        val parser = string(expected)
        val result = parser.parse(input).asFailed
        result.state.position mustEqual 0
        result.error mustEqual s"could not parse '$expected'"
  }

  "take" - {

    "must return exactly as much input as requested" in:

      forAll(Gen.alphaNumStr): input =>
        val parser = take(input.length)
        val result = parser.parse(input).asCompleted
        result.result mustEqual input
        result.state.position mustEqual input.length

    "must fail when there isn't enough input" in:
      forAll(Gen.alphaNumStr): input =>
        val parser = take(input.length + 1)
        val result = parser.parse(input).asFailed
        result.error mustEqual "no more input"
        result.state.position mustEqual 0
  }

  "takeWhile" - {

    "must return a substring of the input while the given predicate holds" in:
      val parser = takeWhile(_.isLetter)
      val result = parser.parse("abc123").asCompleted
      result.result mustEqual "abc"
      result.state.position mustEqual 3
  }
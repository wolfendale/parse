package io.github.wolfendale.parser.builder

import cats.Eval
import cats.syntax.all.*
import io.github.wolfendale.parser.{ParseError, ParseResultExtensions, ParseState, StringParseInput, builder}
import org.scalacheck.{Gen, Shrink}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class TextParsersSpec extends AnyFreeSpec, Matchers, ScalaCheckDrivenPropertyChecks, ParseResultExtensions:

  given [A]: Shrink[A] =
    Shrink.shrinkAny

  import builder.default.*

  "anyChar" - {

    "must parse any char in the input" in:

      val inputGen = for
        length   <- Gen.chooseNum(1, 100)
        input    <- Gen.stringOfN(length, Gen.alphaNumChar)
        position <- Gen.chooseNum(0, input.length - 1)
      yield (input, position)

      forAll(inputGen): (input, position) =>
        val result = anyChar.parse(ParseState[Eval](StringParseInput(input, position))).value
        result.result mustEqual input(position)
        result.state.input.position.value mustEqual position + 1

    "must fail if there is no more input" in:

      val inputGen = for
        length   <- Gen.chooseNum(0, 100)
        input    <- Gen.stringOfN(length, Gen.alphaNumChar)
        position <- Gen.chooseNum(length, length + 100)
      yield (input, position)

      forAll(inputGen): (input, position) =>
        val result = anyChar.parse(ParseState[Eval](StringParseInput(input, position))).value
        result.state.input.position.value mustEqual position
        result.result mustEqual ParseError.UnexpectedEnd
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
        val result = parser.parse(ParseState(StringParseInput[Eval](input, position))).value
        result.result mustEqual expected
        result.state.input.position.value mustEqual position + 1

    "must fail if the given char is not the next char to be parsed in the input" in:

      val inputGen = for
        length   <- Gen.chooseNum(1, 100)
        input    <- Gen.stringOfN(length, Gen.alphaNumChar)
        position <- Gen.chooseNum(0, length - 1)
        expected <- Gen.alphaNumChar.suchThat(_ != input(position))
      yield (input, position, expected)

      forAll(inputGen): (input, position, expected) =>
        val parser = char(expected)
        val result = parser.parse(ParseState(StringParseInput[Eval](input, position))).value
        result.state.input.position.value mustEqual position
        result.result mustEqual ParseError(s"'${input(position)}' was not '$expected'")

    "must fail if there is no more input to be consumed" in:

      val inputGen = for
        length   <- Gen.chooseNum(0, 100)
        input    <- Gen.stringOfN(length, Gen.alphaNumChar)
        position <- Gen.chooseNum(input.length, input.length + 100)
        c        <- Gen.alphaNumChar
      yield (input, position, c)

      forAll(inputGen): (input, position, c) =>
        val parser = char(c)
        val result = parser.parse(ParseState(StringParseInput[Eval](input, position))).value
        result.state.input.position.value mustEqual position
        result.result mustEqual ParseError.UnexpectedEnd
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
        val result = parser.parse(ParseState[Eval](StringParseInput(input, position))).value
        result.result mustEqual expected
        result.state.input.position.value mustEqual position + expected.length

    "must fail if the expected string is not next in the input" in:

      val inputGen = for
        expected <- Gen.alphaNumStr
        input    <- Gen.alphaNumStr.suchThat(!_.startsWith(expected))
      yield (input, expected)

      forAll(inputGen): (input, expected) =>
        val parser = string(expected)
        val result = parser.parse(input).value
        result.state.input.position.value mustEqual 0
        result.result mustEqual ParseError(s"'${input.substring(0, input.length min expected.length)}' was not '$expected'")
  }

  "take" - {

    "must return exactly as much input as requested" in:

      forAll(Gen.alphaNumStr): input =>
        val parser = take(input.length)
        val result = parser.parse(input).value
        result.result mustEqual input
        result.state.input.position.value mustEqual input.length

    "must fail when there isn't enough input" in:
      forAll(Gen.alphaNumStr): input =>
        val parser = take(input.length + 1)
        val result = parser.parse(input).value
        result.result mustEqual ParseError.UnexpectedEnd
        result.state.input.position.value mustEqual 0
  }

  "takeWhile" - {

    "must return a substring of the input while the given predicate holds" in:
      val parser = takeWhile(_.isLetter)
      val result = parser.parse("abc123").value
      result.result mustEqual "abc"
      result.state.input.position.value mustEqual 3
  }

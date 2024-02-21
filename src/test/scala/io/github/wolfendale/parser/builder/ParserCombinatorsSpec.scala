package io.github.wolfendale.parser.builder

import cats.Eval
import cats.syntax.all.*
import io.github.wolfendale.parser.{ParseError, ParseInput, ParseResultExtensions, ParseState, ParserT, StringParseInput, builder}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Gen, Shrink}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParserCombinatorsSpec extends AnyFreeSpec, Matchers, ScalaCheckPropertyChecks, ParseResultExtensions:

  given [A]: Shrink[A] = Shrink.shrinkAny
  
  import builder.default.*

  "success" - {

    "must always succeed" in:

      val parser = success(())

      forAll(arbitrary[String]): input =>
        parser.parse(input).value.asCompleted
  }

  "failed" - {

    "must always fail" in:

      forAll(arbitrary[String], arbitrary[String]): (input, error) =>
        val parser: ParserT[Eval, Unit] = failed(ParseError(error))
        parser.parse(input).value.asFailed.error mustEqual error
  }

  "lookahead" - {

    "must return the result of the wrapped parser without incrementing the parsing position" in:
      val parser = lookahead(string("foo"))
      val result = parser.parse("foo").value
      result.result mustEqual "foo"
      result.state.input.position.value mustEqual 0
  }

  "matches" - {

    val parser = matches(string("foo"))

    "must return true if the underlying parser matches the input, without incrementing position" in:
      val result = parser.parse("foo").value
      result.result mustBe true
      result.state.input.position.value mustEqual 0

    "must return false if the underlying parser does not match the input" in:
      val result = parser.parse("bar").value
      result.result mustBe false
      result.state.input.position.value mustEqual 0
  }

  "zeroOrMore" - {

    val parser = zeroOrMore(anyChar)

    "must repeatedly consume the input while the parser succeeds" in:
      val result = parser.parse("abc").value.asCompleted
      result mustEqual Seq('a', 'b', 'c')

    "must succeed with an empty result if the input does not match" in:
      val result = parser.parse("").value.asCompleted
      result mustEqual Seq.empty
  }

  "oneOrMore" - {

    val parser = oneOrMore(anyChar)

    "must repeatedly consume the input while the parser succeeds" in:
      val result = parser.parse("abc").value.asCompleted
      result mustEqual Seq('a', 'b', 'c')

    "must fail if the parser does not match at least once" in:
      parser.parse("").value.result mustBe a[ParseError]
  }

  "end" - {

    "must successfully parse when there is no more input left" in:
      forAll(Gen.alphaNumStr): input =>
        val state = ParseState[Eval](StringParseInput(input, input.length))
        end.parse(state).value.asCompleted

    "must fail to parse if there is input left" in:

      val inputGen = for
        length <- Gen.chooseNum(1, 100)
        input <- Gen.stringOfN(length, Gen.alphaNumChar)
        position <- Gen.chooseNum(0, length - 1)
      yield (input, position)

      forAll(inputGen): (input, position) =>
        val state = ParseState[Eval](StringParseInput(input, position))
        val result = end.parse(state).value.asFailed
        result.error mustEqual "input remaining"

    "must fail to parse if the remaining input is indeterminate" in:

      val input = new ParseInput[Eval]:
        override def position: Eval[Int] = ???
        override def advance(count: Int): Eval[ParseInput[Eval]] = ???
        override def nextChar: Eval[Option[Char]] = ???
        override def nextString(length: Int): Eval[String] = ???
        override def takeWhile(p: Char => Boolean): Eval[String] = ???
        override def remaining: Eval[Option[Int]] = Eval.now(None)

      val state = ParseState(input)

      val result = end.parse(state).value
      result.state mustBe state
      result.asFailed.error mustEqual "indeterminate input left"
  }

  "lazily" - {

    "must parse the same as the underlying parser" in:
      forAll(Gen.alphaNumStr): input =>
        lazily(anyChar).parse(input).value mustEqual anyChar.parse(input).value

    "must allow for right-recursive parsers" in:

      enum Lang:
        case Terminal(c: Char)
        case Recur(t: Terminal, r: Lang)

      import Lang.*

      val terminal: ParserT[Eval, Terminal] =
        anyChar.map(Terminal.apply)

      lazy val lang: ParserT[Eval, Lang] =
        ((terminal <~ char(';')) ~ lazily(lang)).map(Recur.apply) | terminal

      val result = lang.parse("a;b;c").value.asCompleted
      result mustEqual Recur(Terminal('a'), Recur(Terminal('b'), Terminal('c')))
  }

  "repeat" - {

    "must return a collection with the right elements in it when the parser succeeds" in:

      val inputGen = for
        input <- Gen.alphaNumStr
        num   <- Gen.chooseNum(0, input.length)
      yield (input, num)

      forAll(inputGen): (input, n) =>
        val parser = repeat(anyChar, n)
        val result = parser.parse(input).value
        result.result mustEqual input.substring(0, n).toSeq
        result.state.input.position.value mustEqual n

    "must fail if the parser cannot repeat the given number of times" in:

      forAll(Gen.alphaNumStr): input =>
        val parser = repeat(anyChar, input.length + 1)
        val result = parser.parse(input).value
        result.state.input.position.value mustEqual 0
        result.result mustEqual ParseError.UnexpectedEnd
  }
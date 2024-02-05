package wolfendale

import org.scalatest.matchers.must.Matchers

trait ParseResultExtensions:
  self: Matchers =>

  extension [A](result: ParseResult[A])
    
    def asCompleted: ParseResult.Completed[A] =
      result mustBe a[ParseResult.Completed[A]]
      result.asInstanceOf[ParseResult.Completed[A]]
      
    def asFailed: ParseResult.Failed =
      result mustBe a[ParseResult.Failed]
      result.asInstanceOf[ParseResult.Failed]
      
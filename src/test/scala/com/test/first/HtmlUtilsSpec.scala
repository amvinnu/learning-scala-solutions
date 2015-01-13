package com.test.first

import org.scalatest._

class HtmlUtilsSpec extends FlatSpec with ShouldMatchers {

  "The Html Utils object" should "remove single elements" in {
    HtmlUtils.removeMarkup("<br/>") should equal("")
  }

  it should "remove paired elements" in {
    HtmlUtils.removeMarkup("<b>Hi</b>") should equal("Hi")
  }

  it should "have no effect on empty strings" in {
    val empty = true
    HtmlUtils.removeMarkup("").isEmpty should be(empty)
  }

}
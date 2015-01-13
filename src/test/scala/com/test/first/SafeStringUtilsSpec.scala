package com.test.first

import org.scalatest._

class SafeStringUtilsSpec extends FlatSpec with ShouldMatchers {

  "The SafeStrings Utils object" should "trim strings" in {
    SafeStringUtils.trimToNone(" some ") should equal(Some("some"))
  }

  it should "convert null strings to None" in {
    SafeStringUtils.trimToNone(null) should equal(None)
  }

  it should "convert empty strings to None" in {
    SafeStringUtils.trimToNone(" ") should equal(None)
  }

  it should "convert string to int" in {
    SafeStringUtils.convertToInt("123") should equal(Some(123))
  }

  it should "convert unparseable int strings to None" in {
    SafeStringUtils.convertToInt("12a") should equal(None)
  }
  
  it should "convert null int strings to None" in {
    SafeStringUtils.convertToInt(null) should equal(None)
  }

  it should "convert empty int strings to None" in {
    SafeStringUtils.convertToInt(" ") should equal(None)
  }
  
  it should "return None for random strings if size is less than 0" in {
    SafeStringUtils.randomString(-1) should equal(None)
  }
  
  it should "return None for random strings if size is 0" in {
    SafeStringUtils.randomString(-1) should equal(None)
  }
  
  it should "return random strings of desired size and they should only be alpha chars" in {
    val str = SafeStringUtils.randomString(10).getOrElse("")
    str.size should equal(10)
    str.foreach(ch => assert((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z')))
  }
  
  // a) Start by writing tests for the Github report to verify the 
  // correct behavior of each component. How much of the logic in the 
  // application can you test if your computer lacked an internet connection? 
  // You should be able to test most of the logic without being able to 
  // actually connect to the Github site.
  
  
}
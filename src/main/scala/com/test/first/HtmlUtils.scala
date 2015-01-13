package com.test.first

object HtmlUtils {
  def removeMarkup(input: String) = {
    input
      .replaceAll("""</?\w[^>]*>""","")
      .replaceAll("<.*>","")
  }
  
  trait A {
    def testA:Boolean = { println("testing A"); true}
  }
  
  trait B {
    def testB:Boolean = { println("testing B"); false}
  }
  
  trait Lang {
    self : A with B =>
    
    def test: Boolean = {
      testA == true && testB == false
    }
    
  }
  
  def main(args: Array[String]) : Unit = {
    class SomeLangTest extends A with B with Lang 
    
    (new SomeLangTest).test
    
  }
}
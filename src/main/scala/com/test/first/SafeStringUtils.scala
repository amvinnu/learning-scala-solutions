package com.test.first

object SafeStringUtils {
  // Returns a trimmed version of the string wrapped in an Option,
  // or None if the trimmed string is empty.
  def trimToNone(s: String): Option[String] = {
    Option(s) map (_.trim) filterNot (_.isEmpty)
  }

  // Add a method that safely converts a string to an integer, without 
  // throwing an error if the string is unparseable. Write and execute 
  // tests for valid and invalid input. What are the most appropriate 
  // monadic collections to use in this function?
  def convertToInt(s: String): Option[Int] = {
    util.Try { s.toInt } toOption
  }

  // Add a method that returns a randomly generated string of the given size, limited to only upper- and lower-case letters. 
  // Write and execute tests that verify the correct contents are return and that invalid input is handled. Are there any appropriate 
  // monadic collections to use in this function?
  def randomString(size: Int): Option[String] = {
    size match {
      case x if x <= 0 => None
      case x => {
        val rand = util.Random
        def isAlpha(c: Char) = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
        @annotation.tailrec def nextChar: Char = rand.nextPrintableChar match {
          case x if isAlpha(x) => x
          case _ => nextChar
        }

        Some(List.fill(x)(nextChar).mkString)
      }
    }
  }

}
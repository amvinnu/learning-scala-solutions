package com.test.first

import org.json4s._
import org.json4s.jackson.JsonMethods._

object Chap9_5 {

  def main(args: Array[String]): Unit = {
    displayReport("https://api.github.com/repos/scala/scala/issues?state=closed&per_page=10")
  }

  case class Label(name: String) { override def toString = name }
  case class User(login: String)
  case class Issue(number: Int, title: String, comments: Int, user: User, labels: List[Label])

  trait GithubIssueParser {
    def parseJson(jsonStr: String): List[Issue] = {
      val json = parse(jsonStr)
      implicit val formats = DefaultFormats // Brings in default date formats etc.

      json.extract[List[Issue]]
    }
  }

  trait ReportDecorator {
    val minWidthPerColumn = 4
    val truncationIndicator = "*"

    def output(maxWidth: Int, headers: List[String], list: List[Product]): (List[String], List[String], List[List[String]]) = {

      assert((minWidthPerColumn + truncationIndicator.size) * headers.size <= maxWidth)

      var maxLengths = List.fill(headers.size)(0).toArray
      for (item <- list) {
        for (index <- (0 until item.productArity)) {
          val str = item.productElement(index).toString
          maxLengths(index) = math.max(math.max(maxLengths(index), math.max(str.size, headers(index).size)), minWidthPerColumn)
        }
      }

      var totalLength = maxLengths.sum

      while (totalLength > maxWidth) {
        val lenWithIdxArr = maxLengths.zipWithIndex.sortWith { (tp1, tp2) => tp1._1 > tp2._1 }.toBuffer
        // go down the list trimming 10% at a time until < maxWidth

        for ((lenWithIdx, index) <- lenWithIdxArr.zipWithIndex if totalLength > maxWidth) {
          lenWithIdxArr(index) = (math.max((lenWithIdx._1 * 0.9).toInt, minWidthPerColumn), lenWithIdx._2)
          totalLength = lenWithIdxArr.map { _._1 }.sum
        }
        maxLengths = lenWithIdxArr.sortWith { (tp1, tp2) => tp1._2 < tp2._2 }.toArray.map { _._1 }
      }

      def getWithTruncationIndicator(original: String, modified: String) = {
        modified.size match {
          case x if x < original.size => s"${modified.dropRight(truncationIndicator.size)}$truncationIndicator"
          case _ => modified
        }
      }

      val headerStrings = headers.zipWithIndex.map { tp =>
        {
          val str = tp._1.take(maxLengths(tp._2))
          getWithTruncationIndicator(tp._1, str)
        }
      }
      val tupleStrings = list.map(item => {
        var tpList: List[String] = Nil
        for (index <- 0 until item.productArity) {
          val str = item.productElement(index).toString
          val trunc = str.take(maxLengths(index))
          tpList ::= getWithTruncationIndicator(str, trunc)
        }
        tpList.reverse
      })

      val formatStrings = maxLengths.map(x => s"%-${x}s").toList

      (formatStrings, headerStrings, tupleStrings)
    }
  }

  object GithubIssueParser extends GithubIssueParser

  object ReportDecorator extends ReportDecorator

  def displayReport(url: String) = {
    /*
    val max = 40
    val (formats, header, tuples) =
      GithubIssueDecorator.output(
        max,
        "h" * 10 :: "h" * 25 :: Nil,
        ("a" * 10, "b" * 25) :: ("c" * 20, "d" * 45) :: Nil)
    println(s"""${"=" * max}""")
    println(String.format(formats.mkString(" "), header: _*))
    println(s"""${"=" * max}""")
    tuples.foreach(x => {
      println(String.format(formats.mkString(" "), x: _*))
    })
    */

    val issueList = GithubIssueParser.parseJson(io.Source.fromURL(url).mkString)
    val headers = "Number" :: "Title" :: "Comments" :: "Labels" :: Nil
    val issues = issueList.map(issue => (issue.number.toString, issue.title, issue.comments, issue.labels.mkString(",")))
    val max = 80
    val (formats, header, tuples) =
      ReportDecorator.output(
        max,
        headers,
        issues)
    println(s"""${"=" * max}""")
    //println(formats.mkString(" "))
    println(String.format(formats.mkString(" "), header: _*))
    println(s"""${"=" * max}""")
    tuples.foreach(x => {
      println(String.format(formats.mkString(" "), x: _*))
    })

  }
}
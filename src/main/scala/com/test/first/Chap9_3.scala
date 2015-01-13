package com.test.first

object Chap9_3 {

  def main(arr: Array[String]): Unit = {

    // "-pattern" :: "abc*" :: "-replacement" :: "xyz" :: "file1" :: Nil
    // "file2" :: "-pattern" :: "abc*" :: "-replacement" :: "xyz" :: "file1" :: Nil
    // "file2" :: "-pattern" :: "abc*" :: "-replacement" :: Nil
    val (pattern, repl, files) = parseArgs(arr)
    
    if(pattern.isEmpty || repl.isEmpty) {
      println("usage: -pattern <pattern> -replacement <replacement> file1 file2 ...")
      return
    }

    println(s"pattern = $pattern, repl = $repl, files = $files")
    val regex = pattern.get.r

    files.foreach(file => {
      val lines = scala.io.Source.fromFile(file).getLines
      var result: List[String] = Nil
      lines.foreach(line => {
        result ::= regex.replaceAllIn(line, repl.get)
      })

      println("replaced content: " + result.reverse.mkString("\n"))
    })
  }

  def parseArgs(args: Array[String]) = {
    var pattern: Option[String] = None
    var repl: Option[String] = None
    var files: List[String] = Nil

    def getArgValue(index: Int) = {
      index match {
        case x if x == (args.size - 1) => None
        case _ => Some(args(index + 1))
      }
    }

    for ((arg, index) <- args.zipWithIndex) {
      arg match {
        case "-pattern" => pattern = getArgValue(index)
        case "-replacement" => repl = getArgValue(index)
        case x => files ::= x
      }
    }

    var filtered: Set[String] = Set()

    def addFiltered(arg: Option[String]) =
      arg match {
        case Some(x) => filtered += x
        case _ =>
      }

    addFiltered(pattern)
    addFiltered(repl)

    files = files.filterNot(x => filtered.contains(x))

    (pattern, repl, files)
  }

}
package com.test.first

object Chap7 {

  def main(arr: Array[String]): Unit = {

    testQ6

  }

  private def testQ6 = {
    val url = "https://github.com/scala/scala/commits/2.11.x.atom"

    // a) Move the user, repo and branch parameters into a tuple parameter. 

    def getCommits(urp: (String, String, String)): List[(String, java.util.Date, String)] = {
      val url = s"""https://github.com/${urp._1}/${urp._2}/commits/${urp._3}.atom"""
      val src = io.Source.fromURL(url)
      val lines = src.getLines.map(_.trim)

      //lines.foreach( println(_) )

      val allContent = lines.mkString("")
      val entries = allContent.split("<entry>").toList.drop(1) // lose the head token as it is the xml header before the first entry

      val df = new java.text.SimpleDateFormat("""yyyy-MM-dd'T'HH:mm:ssXXX""")
      val pattern = """.*<title>(.*)</title>.*<updated>(.*)</updated>.*<author><name>(.*)</name>.*""".r

      entries.map(entry => {
        val pattern(title, updated, author) = entry
        (title, df.parse(updated), author)
      }) // .foreach(x => println(s"title: ${x._1}, updated: ${of.format(x._2)}, author: ${x._3}"))
    }

    // b) Following exercise "a", have the function take a list of Github projects and print a report of each one’s commits, in order of specified project. 

    val projects = ("akka", "akka", "master") :: ("scala", "scala", "2.11.x") :: ("sbt", "sbt", "0.13") :: ("scalaz", "scalaz", "series/7.2.x") :: Nil
    projects.foreach(urp => {
      println(f"""${"=" * 144}\n${urp._1}%72s\n${"=" * 144}""")
      val of = new java.text.SimpleDateFormat("""EEE MMM dd HH:mm:ss zzz yyyy""")
      getCommits(urp).foreach(x => println(f"${x._1}%-96s ${of.format(x._2)}%-32s ${x._3}%-16s"))
    })

    // c) Following exercise "b", retrieve all of the projects commit data concurrently using futures, await the result (no more than 5 seconds), and then 
    // print a commit report for each project, in order of project specified. 

    import concurrent.Future
    import concurrent.ExecutionContext.Implicits.global
    import java.util.Date

    val reqs = Future sequence (projects.map(urp => Future((urp._1, getCommits(urp)))))
    concurrent.Await.result(reqs, concurrent.duration.Duration(5, concurrent.duration.SECONDS)).foreach({
      case (project: String, commits: List[(String, Date, String)]) => {
        println(f"""${"=" * 144}\n$project%72s\n${"=" * 144}""")
        val of = new java.text.SimpleDateFormat("""EEE MMM dd HH:mm:ss zzz yyyy""")
        commits.foreach(commit => println(f"${commit._1}%-96s ${of.format(commit._2)}%-32s ${commit._3}%-16s"))
      }
    })

    // d) Following exercise "c", mix the commits together and sort by commit date, then print your report with an additional "repo" column.
    
    val mixed = Future sequence (projects.map(urp => Future((urp._2, getCommits(urp)))))
    val allCommits = concurrent.Await.result(mixed, concurrent.duration.Duration(5, concurrent.duration.SECONDS)).flatMap({
      case (repo: String, commits: List[(String, Date, String)]) => {
        commits.map(commit => (commit._1, commit._2, commit._3, repo))
      }
    }).sortWith((tp1, tp2) => tp1._2.compareTo(tp2._2) < 0)
    println(f"""${"=" * 160}\n${"Title"}%72s${"Date"}%32s${"Author"}%32s${"Repo"}%16s\n${"=" * 160}""")
    val of = new java.text.SimpleDateFormat("""EEE MMM dd HH:mm:ss zzz yyyy""")
    allCommits.foreach({
      case (title, date, author, repo) => println(f"$title%-96s ${of.format(date)}%-32s $author%-16s $repo")
    })

  }

  private def testQ5 = {
    def getEnv(prop: String): Option[String] = {
      util.Try(System.getenv(prop)) match {
        case util.Success(x) => Option(x) // will return None for x = null and Some for other x
        case util.Failure(ex) => None
      }
    }

    println(getEnv("LANG"))
    println(getEnv(null))
    println(getEnv("whatever"))
  }

  private def testQ4 = {
    def prod(x: String, y: String): Option[AnyVal] = {
      val xc = util.Try(x.toInt) orElse util.Try(x.toFloat)
      val yc = util.Try(y.toInt) orElse util.Try(y.toFloat)

      (xc, yc) match {
        case (util.Success(xc: Int), util.Success(yc: Int)) => Some(xc * yc)
        case (util.Success(xc), util.Success(yc)) => Some(x.toFloat * y.toFloat)
        case _ => None
      }
    }

    println(prod("100", "20"))
    println(prod("110.01", "100"))
    println(prod("abc", "100"))
  }

  private def testQ2_3 = {

    def getFiles(dir: String): Array[String] = {
      val files: Array[java.io.File] = new java.io.File(dir).listFiles
      val pattern = """.*/([^/]+)$""".r
      files.map(file => {
        val pattern(name) = file.toString
        name
      })
        .filterNot(_.startsWith("."))
    }

    val files = getFiles(".")
    println(files.mkString(":"))

    val map = files.foldLeft(collection.mutable.Map[String, Int]())(
      (map, file) => {
        map += (file.take(1) -> (map.getOrElse(file.take(1), 0) + 1))
      })

    println(map)

  }

  private def testQ1 = {
    // a) Write a function that returns a list of the first x elements in the Fibonacci series Can you write this with a Buffer? Would a Builder be appropriate here?

    def fib(count: Int): List[Int] = {
      val result = collection.mutable.Buffer[Int]()

      result ++= 1 :: 1 :: Nil
      (2 to count).foreach(idx => result += (result(idx - 1) + result(idx - 2)))

      count match {
        case x if x > 0 => result.take(count).toList
        case _ => List.empty[Int]
      }
    }

    println(fib(10))

    // b) Write a new Fibonacci function that adds new Fibonacci numbers to an existing list of numbers. It should take a list of numbers (List[Int]) and the count of new 
    // elements to add and return a new list (List[Int]). While the input list and returned lists are immutable, you should be able to use a mutable list inside your function. 
    // Can you also write this function using only immutable lists? Which version, using mutable vs immutable collections, is more appropriate and readable?

    def fibAdd(l: List[Int], count: Int): List[Int] = {
      val result = collection.mutable.Buffer[Int]()

      l match {
        case Nil => result ++= 1 :: 1 :: Nil
        case x :: Nil => result ++= x :: 1 :: Nil
        case _ => result ++= l
      }

      val lastIdx = result.size - 1
      (1 to count).foreach(idx => result += (result(lastIdx + idx - 1) + result(lastIdx + idx - 2)))

      result.take(l.size + count).toList
    }

    println(fibAdd(fib(10), 5))
    println(fibAdd(fib(0), 5))
    println(fibAdd(fib(1), 5))

    // c) The Stream collection is a great solution for creating a Fibonacci series. Create a stream that will generate a Fibonacci series. Use it to print out the first 100 
    // elements in the series, in a formatted report of 10 comma-delimited elements per line.

    import math.BigInt
    def streamFib: Stream[BigInt] = {

      def recurse(penultimate: BigInt, last: BigInt): Stream[BigInt] = {
        (penultimate + last) #:: recurse(last, penultimate + last)
      }

      1 #:: 1 #:: recurse(1, 1)
    }

    streamFib.take(100).toList.grouped(10).foreach { x: List[BigInt] => println(x.mkString(",")) }

    // d) Write a function that takes an element in the Fibonacci series and returns the following element in the series. For example, fibNext(8) should return 13. 
    // How will you handle invalid input such as fixNext(9) ? What are your options for conveying the lack of a return value to callers?

    def fibNext(fib: BigInt): Option[BigInt] = {
      val fibList = streamFib.takeWhile(_ <= fib).toList
      fibList.last match {
        case `fib` => Some(fibList.takeRight(2).sum)
        case _ => None
      }
    }

    println(fibNext(8))
    println(fibNext(9))
  }
}
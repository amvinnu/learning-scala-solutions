package com.test.first

object Chap1toChap6 {

  def main(arr: Array[String]): Unit = {
    
    testChap6Extra
    
  }
  
  private def testChap6Extra = {
    val url = "http://api.openweathermap.org/data/2.5/forecast?mode=xml&lat=55&lon=0"
    val lines = io.Source.fromURL(url, "utf-8").getLines.toList
    
    // a) To make doubly sure we have the right content, print out the top 10 lines of the file. This should be a one-liner.
    lines.take(100).foreach(println(_))
    
    // b) The forecast’s city’s name is there in the first 10 lines. Grab it from the correct line and print out its xml element. 
    // Then extract the city name and country code from their xml elements and print them out together (e.g., "Paris, FR"). This is a 
    // good place to use regular expressions to extract the text from xml tags (see “Regular Expressions”).
    
    val nameAndCountryPattern = """\s*<(name|country)>(\w+)</(name|country)>""".r
    println(lines.take(10).map(_ match {
      case nameAndCountryPattern(_,m,_) => m
      case _ => ""
    }).filterNot(_.isEmpty).mkString(","))
 
    // c) How many forecast segments are there? What is the shortest expression you can write to count the segments?
    val forecastSegmentPattern = """\s*<time from="([^\s]+)" to="([^\s]+)">""".r
    println(lines.filter( _ match { 
      case forecastSegmentPattern(_,_) => true
      case  _ => false
    }).size)

    // d) The "symbol" xml element in each forecast segment includes a description of the weather forecast. Extract this element in the same way you extracted the city name and 
    // country code. Try iterating through the forecasts, printing out the description.
    // Then create an informal weather report by printing out the weather descriptions over the next 12 hours (not including the xml elements).
    val df = new java.text.SimpleDateFormat("""yyyy-MM-dd'T'HH:mm:ss""")
    val of = new java.text.SimpleDateFormat("""EEE MMM dd HH:mm:ss zzz yyyy""")
    import java.util.{Calendar, Date}
    val cal = Calendar.getInstance
    cal.add(Calendar.HOUR, 12)
    
    val symbolNumberNamePattern = """\s*<symbol number="([^"]+)" name="([\w\s]+)" var="[^"]+"/>""".r
    val forecasts = collection.mutable.ListBuffer[(Date, Date, StringBuilder)]()
    lines.foreach( _ match { 
      case forecastSegmentPattern(start, end) => forecasts += Tuple3(df.parse(start), df.parse(end), new StringBuilder)
      case symbolNumberNamePattern(_, symbol) => forecasts.last._3.append(symbol)
      case  _ => (null, null, null)
    })
    forecasts.filterNot(_ match {
      case (null, _, _) => true
      case (start, end, _) if start.after(cal.getTime) => true
      case _ => false
    }).map( {
       	// Partial function so we can refer to individual tuple values
        case (start, end, symbol) => (of.format(start), of.format(end), symbol)
    })
    .foreach(x => println(s"from: ${x._1} to: ${x._2} forecast: ${x._3}"))
    
    // e) Let’s find out what descriptions are used in this forecast. Print a sorted listing of all of these descriptions in the forecast, with duplicate entries removed.
    lines.map( _ match { 
      case symbolNumberNamePattern(_, symbol) => symbol
      case _ => ""
    })
    .filterNot(_.isEmpty)
    .distinct
    .sortWith(_.compareToIgnoreCase(_) < 0) // or sorted
    .foreach(println(_))
    
    // f) These descriptions may be useful later. Included in the "symbol" xml element is an attribute containing the symbol number. Create a Map from the symbol number to 
    // the description. Verify this is accurate by manually accessing symbol values from the forecast and checking that the description matches the xml document.
    lines.map( _ match { 
      case symbolNumberNamePattern(number, symbol) => (number -> symbol)
      case _ => (null.asInstanceOf[String] -> null.asInstanceOf[String])
    })
    .filter(_._1 != null)
    .toMap
    .foreach(x => println(s"${x._1} -> ${x._2}"))
    
    // g) What are the high and low temperatures over the next 24 hours?
    val cal2 = Calendar.getInstance
    cal2.add(Calendar.HOUR, 24)
    val temperaturePattern = """\s*<temperature unit="[^"]+" value="([\d.]+)" min="[^"]+" max="[^"]+"/>""".r
    val temps = collection.mutable.ListBuffer[(Date, Date, StringBuilder)]()
    lines.foreach( _ match { 
      case forecastSegmentPattern(start, end) => temps += Tuple3(df.parse(start), df.parse(end), new StringBuilder)
      case temperaturePattern(temp) => temps.last._3.append(temp)
      case  _ => (null, null, null)
    })
    val (min, max) = temps.filterNot(_ match {
      case (null, _, _) => true
      case (start, end, _) if start.after(cal2.getTime) => true
      case _ => false
    })
    .map(_._3.toDouble)
    .foldLeft((Double.MaxValue, Double.MinValue))({
      // partial function
      case ((min, max), current) => (math.min(min, current), math.max(max, current)) 
    })
    println(s"min: $min, max: $max")
    
    // h) What is the average temperature in this weather forecast? You can use the "value" attribute in the temperature element to calculate this value.
    val allTemps:List[Double] = lines.map( _ match { 
      case temperaturePattern(temp) => temp
      case  _ => null
    })
    .filter(_ != null)
    .map(_.toDouble)
    
    allTemps match {
      case Nil => println("avg temp: n/a")
      case List(_*) => println(f"avg: ${allTemps.sum / allTemps.size}%.2f")
    }
    
    
    
    
  }
  
  private def testChap6 = {
    // Create a list of the first 20 odd Long numbers. Can you create this with a for-loop, with the filter operation, and with the map operation? What’s the most efficient and expressive way to write this?
    val l1 = for(i <- 1 to 40 if i%2 != 0) yield i
    println(l1)
    val l2 = (1 to 40) filter(_ % 2 != 0)
    println(l2)
    val l3 = (0 until 20) map(2 * _ + 1)
    println(l3)
    
    // Write a function titled "factors" that takes a number and returns a list of its factors, other than 1 and the number itself. For example, factors(15) should return List(3, 5).
    // Then write a new function that applies "factors" to a list of numbers. Try using the list of Long numbers you generated in exercise 1. 
    // For example, executing this function with the List(9, 11, 13, 15) should return List(3, 3, 5), as the factor of 9 is 3 while the factors of 15 are 3 again and 5. Is this a good place to use map and flatten? Or, would a for-loop be a better fit?
    
    def factors(x: Int):List[Int] = {
      (2 to x/2.toInt).filter(x % _ == 0).toList
    }
    
    println(factors(15))
    println(l3.map(factors(_)).filter(!_.isEmpty).distinct)
    println(l3.map(factors(_)).flatten.distinct)
    println(l3.flatMap(factors(_)).distinct)
    
    // Write a function, first[A](items: List[A], count: Int): List[A], that returns the first x number of items in a given list. For example, first(List('a','t','o'), 2) should 
    // return List('a','t'). You could make this a one-liner by invoking one of the built-in list operations that already performs this task, or (preferably) implement your own 
    // solution. Can you do so with a for-loop? With foldLeft? With a recursive function that only accessed head and tail?
    
    def first[A](l:List[A], count: Int):List[A] = {
      // l.take(count)
      // (for(i <- 0 until count) yield l(i)).toList
      // l.foldLeft(collection.mutable.ListBuffer[A]())((x,y) => if(x.size < count) x += y else x).toList
      
      if(count > 1) {
        l.head :: first(l.tail, count - 1)
      } else {
        List(l.head)
      }
    }
    
    println(first('a' :: 't' :: 'o' :: Nil, 2))
    
    // Write a function that takes a list of strings and returns the longest string in the list. Can you avoid using mutable variables here? This is an excellent candidate for the 
    // list-folding operations (Table 6-5) we studied. Can you implement this with both fold and reduce ? Would your function be more useful if it took a function parameter that 
    // compared two strings and returned the preferred one? How about if this function was applicable to generic lists, ie lists of any type?
    
    def longest[A](l:List[A], f: (A,A) => Boolean):A = {
      l.fold((null.asInstanceOf[A]))((x:A,y:A) => if(f(x,y)) x else y)
    }
    
    println(longest("a" :: "aaaa" :: "aa" :: "aaa" :: Nil, (x:String, y:String) => x != null && x.length > y.length()))
    
    // Write a function that reverses a list. Can you write this as a recursive function? This may be a good place for a match expression.
    
    def reverse[A](l:List[A]):List[A] = {
      l match {
        case x :: Nil => List(x)
        case x :: xt => reverse(xt) :+ x
        case _ => l // to match the empty case
      }
    }
    
    println(reverse('a' :: 'e' :: 'i' :: 'o' :: 'u' :: Nil))
    println(reverse(Nil))
    
    // Write a function that takes a List[String] and returns a (List[String],List[String]), a tuple of string lists. The first list should be items in the original list that 
    // are palindromes (written the same forwards and backwards, like "racecar"). The second list in the tuple should be all of the remaining items from the original list. You 
    // can implement this easily with partition, but are there other operations you could use instead?
    
    def palpartition(l: List[String]):(List[String], List[String]) = {
      // l.partition(x => x == x.reverse)
      
      l.foldLeft((List.empty[String], List.empty[String])) {
          // using a partial function here so we can identify tuple parts by param names pals & nonpals
          case ((pals:List[String], nonpals:List[String]), arg:String) => arg match {
            case x if arg == arg.reverse => (arg :: pals, nonpals)
            case x => (pals, arg :: nonpals)
          } 
      }
      
      l.foldLeft((List.empty[String], List.empty[String])) {
          // without partial function
          (acc:(List[String], List[String]), arg:String) => arg match {
            case x if arg == arg.reverse => (arg :: acc._1, acc._2)
            case x => (acc._1, arg :: acc._2)
          } 
      }
    }
    
    println(palpartition("abba" :: "whatever" :: "racecar" :: Nil))
    
  }
  
  private def testChap5 = { 
    val max: (Int, Int) => Int = { (x, y) => if(x > y) x else y }
    def tupleMax[T : math.Ordering](x: Tuple3[T, T, T], maxer: (T, T) => T) : T = {
      println(s"computing max among $x")
      maxer(maxer(x._1, x._2), x._3)
    }
    
    //println(tupleMax((util.Random.nextInt(100), util.Random.nextInt(100), util.Random.nextInt(100)), max))
    
    def ho(x:Int): Int => Int = y => x * y
    
    //println(ho(3)(5))
    
    def square(m: Double) = m * m
    val sq : Double => Double = square
    
    def conditional[T](x:T, p: T => Boolean, f: T => T ): T = {
      if(p(x)) f(x)
      else x
    }
    
    //println(conditional(7, { x:Int => if (x > 5) true else false }, { x:Int => x * 2}))
    
    /*
    (1 to 100).foreach {
      x:Int => {
      	if     (conditional(x, { x:Int => if(x % 15 == 0) true else false}, { x:Int => println("typesafe"); x * 15 }) == x * 15) {}
      	else if(conditional(x, { x:Int => if(x % 3 == 0)  true else false}, { x:Int => println("type"); x * 3}) == x * 3) {}
      	else if(conditional(x, { x:Int => if(x % 5 == 0)  true else false}, { x:Int => println("safe"); x * 5}) == x) {}
      	else println(x)
      }
    }
    */
    
    def conditional2[T](x:T, p: T => Boolean, f: T => Unit ): Boolean = {
      if(p(x)) { f(x); true }
      else false
    }
    
    (1 to 100).foreach {
      x:Int => {
      	if     (conditional2(x, { x:Int => x % 15 == 0 }, { x:Int => println("typesafe") })) {}
      	else if(conditional2(x, { x:Int => x % 3 == 0  }, { x:Int => println("type") })) {}
      	else if(conditional2(x, { x:Int => x % 5 == 0  }, { x:Int => println("safe") })) {}
      	else println(x)
      }
    }
  }
  
  private def testChap4 = {
    def area(r:String) = { val x = r.toInt; 3.14 * x * x}
    
    //println(area("1"))
    // println(area(""))
    
    
    @annotation.tailrec def printNum(n:Int):Unit = {
      print(f"$n%2s, ")
      if(n > 5 && n % 5 == 0) println
      if(n < 50)
        printNum(n+1)
    }
    
    //printNum(5)
    
    @annotation.tailrec def pow(x:Int, y:Int, acc: Long):Long = {
      if(y > 0) {
        pow(x, y -1, acc * x)
      } else {
        acc
      }
    }
    
    println(pow(2, 10, 1))
    
    def tdiff(x:Tuple2[Int, Int], y:Tuple2[Int, Int]):Tuple2[Double, Double] = {
      (math.abs(x._1 - y._1), math.abs(x._2 - y._2))
    }
    
    println(tdiff((3,4), (7,8)))
    
  }
  
  private def testChap3 = {
  	val name:String = null
  	val n = name match {
  	  case x if x == null || x.trim.isEmpty => "n/a"
  	  case _ => name
  	}
  	println(n)
  	
  	val amount:Double = 0
  	val a = amount match {
  	  case x if x > 0 => "greater"
  	  case x if x == 0 => "equal"
  	  case _ => "lesser"
  	}
  	println(a)
  	
  	//(1 to 100).foreach( x => { print(f"$x%3s, "); if(x % 5 == 0) println  } )
  	
  	(1 to 100).foreach( x => {
  	  x match {
  	    case n if n % 15 == 0 => println("typesafe")
  	    case n if n % 3 == 0 => println("type")
  	    case n if n % 5 == 0 => println("safe")
  	    case n => println(n)
  	  }
  	})
  }
  
  private def testChap2 = {
    // Using the input value 2.7255, generate the string "You owe $2.73 dollars". Is this doable with string interpolation?
    
    val x:Double = 2.7255
    val msg = f"You owe $x%.2f dollars"
    //println(msg)
    
    // Using the input string "Frank,123 Main,925-555-1943,95122" and regular expression matching, retrieve the telephone number. 
    // Can you convert each part of the telephone number to its own integer value? How would you store this in a tuple?
    
    val pattern = """([\w]+),([\d\w\s]+),([\d-]+),([\d]+)""".r
    val pattern(name, addr, phone, zip) = "Frank,123 Main,925-555-1943,95122"
    println(s"name = $name, addr = $addr, phone = $phone, zip = $zip")
    
    val parts:Array[Int] = phone.split("-").map(_ toInt)
    parts.foreach(println _)
  }
  
  private def testUrl = {
    val url = "http://api.openweathermap.org/data/2.5/forecast?mode=xml&lat=55&lon=0"
    val l: List[String] = io.Source.fromURL(url).getLines.toList
    
    println( l(0) )
  }
}
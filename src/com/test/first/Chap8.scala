package com.test.first

object Chap8 {

  def main(arr: Array[String]): Unit = {

    testQ4

  }

  sealed class MidiPlayer(private [first] val volume: Int) {
    private[this] val duration = 250
    lazy private[this] val synth = javax.sound.midi.MidiSystem.getSynthesizer

    def play(notes: Seq[Int]): Unit = {
      synth.open
      val channel = synth.getChannels.head
      notes.foreach{ x => channel.noteOn(x, duration); Thread.sleep(250); }
      channel.allNotesOff
      synth.close
    }

    def play(notes: Range): Unit = {
      play(notes.toList)
    }

    def play(notes: List[Range]): Unit = {
      play(notes.flatten)
    }
  }
  
  object MidiPlayer extends MidiPlayer(95)

  private def testQ4 = {
    // Create a simpler interface to this by writing a class that plays a series of notes. The class’s constructor should take the volume (set to 80 in the example) 
    // but always use the same duration (250 milliseconds in the example) . Its "play" method should take a list of the notes, 
    // for example Seq(30, 35, 40, 45, 50, 55, 60, 65, 70), and play them in the synthesizer.

    // Assume the getSynthesizer method call is expensive. How can you prevent unnecessarily calling it in case the "play" method is never called?
    // Make sure to hide fields that callers don’t need to know about
    // Can you support a Range as input, eg play(30 to 70 by 5) ?
    // Can you support multiple ranges, for example a series of notes that rise, fall, and then rise again?
    // Assume we only ever need one instance, ever, with the volume set to 95. Can you use access controls to ensure that there will never be more than one instance of this class?

    MidiPlayer.play(Range(30, 70, 5) :: Range(70, 30, -5) :: Range(40, 90, 5) :: Range(90, 40, -5) :: Nil)
  }

  private def testQ3 = {

    class DirListing(dirName: String, f: String => Boolean) {
      lazy val dir = new java.io.File(dirName)
      lazy val filter = new java.io.FilenameFilter() {
        def accept(dir: java.io.File, name: String): Boolean = {
          f(name)
        }
      }

      def list: List[String] = {
        dir.listFiles(filter).map(file => file.getName()).toList
      }
    }

    println(new DirListing(".", { !_.startsWith(".") }).list)

  }

  private def testQ2 = {
    abstract class LListBase[+T] {
      def tail: LListBase[T]
      def head: T
      def foreach[B](f: T => B): Unit
    }

    sealed class LListLast extends LListBase[Nothing] {
      override def tail: LListBase[Nothing] = throw new NoSuchElementException("empty list")
      override def head: Nothing = { println(this); throw new NoSuchElementException("empty list") }
      override def foreach[B](f: Nothing => B) = throw new NoSuchElementException("empty list")
    }

    object LListLast extends LListLast

    class LList[T](elems: Array[T]) extends LListBase[T] {

      override def head = elems.isEmpty match {
        case true => LListLast.head
        case _ => {
          elems.head
        }
      }

      override def tail = {
        val x = elems.slice(1, elems.size)
        x.isEmpty match {
          case true => LListLast
          case _ => {
            new LList(x)
          }
        }
      }

      override def foreach[B](f: T => B) = {
        var x: LListBase[T] = this
        while (x != LListLast) {
          f(x.head)
          x = x.tail
        }
      }
    }

    val l = new LList(Array("a", "b"))
    l.foreach(println)
    //println(l.head)
    //println(l.tail.head)
    //println(l.tail.tail.head)

  }

  private def testQ1 = {

    // a) Create a console class that can track the make, model, debut date, wifi type, physical media formats supported, and maximum video resolution. 
    // Override the default toString method to print a reasonably-sized description of the instance (< 120 chars).
    // - The debut date (or launch date) should be an instance of java.util.Date
    // - Keep the wifi type (b/g, b/g/n, etc) field optional, in case some consoles don’t have wifi.
    // - The physical media formats should be a list. Is a String the best bet here, or an Int that matches a constant value?
    // - The maximum video resolution should be in a format that would make it possible to sort consoles in order of greatest number of pixels.

    object WifiType extends Enumeration {
      type WifiType = Value
      val none = Value("none")
      val bg = Value("b/g")
      val bgn = Value("b/g/n")
    }

    object MediaFormat extends Enumeration {
      type MediaFormat = Value
      val none = Value("none")
      val dvd = Value("DVD")
      val bluray = Value("Bluray")
    }

    import java.util.Date
    import WifiType.WifiType
    import MediaFormat.MediaFormat
    class Console(
        val make: String, val model: String, val debut: Date, val formats: List[MediaFormat],
        val resolution: (Int, Int), val wifi: WifiType = WifiType.none) {

      override def equals(other: Any): Boolean = {
        other.getClass() == this.getClass && make == other.asInstanceOf[Console].make && model == other.asInstanceOf[Console].model
      }

      override def hashCode = {
        var code = make.hashCode
        code += code * 41 + model.hashCode
        code
      }

      override def toString = {
        val of = new java.text.SimpleDateFormat("""MM/dd/yyyy""")
        val result = s"make: $make, model: $model, debut: ${of.format(debut)}, formats: ${formats.mkString(",")}, " +
          s"resolution: ${resolution._1}x${resolution._2}, wifi: ${wifi}"
        result.length match {
          case x if x <= 120 => result
          case _ => result.substring(0, 118) + ".."
        }
      }
    }

    // b) Test our your new console class by writing a new class that creates 4 instances of this console class. All of the instances should have reasonably accurate values.

    class MyConsoles {
      val consoles =
        new Console("Sony", "PlayStation 4", new Date(), MediaFormat.dvd :: MediaFormat.bluray :: Nil, (1920, 1080), WifiType.bgn) ::
          new Console("Sony", "PlayStation 3", new Date(), MediaFormat.dvd :: MediaFormat.bluray :: Nil, (1920, 1080), WifiType.bgn) ::
          new Console("Microsoft", "XBox 360", new Date(), MediaFormat.bluray :: Nil, (720, 1080), WifiType.bg) ::
          new Console("Nintendo", "DS", new Date(), MediaFormat.none :: Nil, (512, 256)) :: Nil
    }

    (new MyConsoles).consoles.foreach(println)

    // c) Now its time for games. Create a game class that includes the name, maker, and a list of consoles it supports, plus an "isSupported" method that returns true 
    // if a given console is supported.

    class Game(val name: String, val maker: String, val consoles: List[Console]) {
      def isSupported(console: Console): Boolean = {
        consoles.find(_ == console) match {
          case Some(x) => true
          case _ => false
        }
      }

      override def toString = s"name: $name, maker: $maker"
    }

    // d) Test out this game class by generating a list of games, each containing one or more instances of consoles. 
    // Can you convert this list to a lookup table of consoles to a list of supported games? How about a function that prints a list of the games, sorted first by maker 
    // and then by game name?
    val games =
      new Game("ActiVision", "Call of Duty: World at War", (new MyConsoles).consoles.drop(1).take(2)) ::
        new Game("ActiVision", "Call of Duty 4: Modern Warfare", (new MyConsoles).consoles.drop(1).take(2)) ::
        new Game("Nintendo", "Pokémon Black and White", (new MyConsoles).consoles.takeRight(1)) :: Nil

    games.foldLeft(collection.mutable.Map[Console, List[Game]]())(
      (map, game) => {
        game.consoles.foreach(
          console => map += (console -> (game :: map.getOrElse(console, List[Game]()))))
        map
      }).foreach(println)

    println(f"""${"=" * 192}\n${"Maker"}%16s${"Name"}%24s${"Console"}%32s\n${"=" * 192}""")
    games.sortWith((x, y) => x.maker.compareToIgnoreCase(y.maker) < 0).sortWith((x, y) => x.name.compareToIgnoreCase(y.name) < 0)
      .foreach(game => println(f"${game.maker}%-32s ${game.name}%-32s ${game.consoles.mkString(":")}"))

  }

}
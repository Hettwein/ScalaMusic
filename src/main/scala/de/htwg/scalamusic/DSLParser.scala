package de.htwg.scalamusic

import scala.util.parsing.combinator.RegexParsers

package object parser {

  object DSLParser extends RegexParsers {

    //defaults
    var lastTime: TimeSignature = TimeSignature()
    var lastKey: Key = MajorScale(Pitch())
    var lastClef: Clef.Value = Clef.treble
    var lastTempo: Int = 100
    var setDefaultTempo: Boolean = true
    def startTempo: Boolean = {
      val tmp = setDefaultTempo
      setDefaultTempo = false
      tmp
    }

    def quantity: Parser[Int] = """(0|[1-9]\d*)""".r ^^ {
      case q => q.toInt
    }

    def duration: Parser[Duration] = """(1|2|4|8|16|32)""".r ~ opt(".") ~ opt(rep1(("~" ~ opt("|")) ~> (element))) ^^ {
      case d ~ None ~ None => Duration(1, d.toInt)
      case d ~ Some(a) ~ None => Duration(3, 2 * d.toInt)
      case d ~ None ~ Some(tn) => Duration(1, d.toInt, tn(0).duration)
      case d ~ Some(a) ~ Some(tn) => Duration(3, 2 * d.toInt, tn(0).duration)
    }

    def element: Parser[MusicElement] = (note | rest | chordName | tuplet) ^^ {
      case e => e
    }

    def pitchClass: Parser[PitchClass.Value] = """([a-g,A-G])""".r ^^ {
      PitchClass(_)
    }

    def pitchDecorator: Parser[PitchDecorator.Value] = """(isis|is|eses|ses|es|s)""".r ^^ {
      PitchDecorator(_)
    }

    def pitchOctave: Parser[Int] = """([,|']*)""".r ^^ {
      case s => s.count(_ == ''') - s.count(_ == ',')
    }

    def pitch: Parser[Pitch] = pitchClass ~ opt(pitchDecorator) ~ opt(pitchOctave) ^^ {
      case c ~ d ~ o => Pitch(c, d.getOrElse(PitchDecorator.Blank), o.getOrElse(0))
    }

    def note: Parser[Note] = pitch ~ duration ^^ {
      case p ~ d => Note(p, d)
    }

    def tuplet: Parser[Tuplet] = "{" ~> (rep1(element) <~ "}") ~ quantity ^^ { // ?
      case m ~ d => Tuplet(d.toInt, m)
    }

    def rest: Parser[Rest] = """(r|R)""".r ~> duration ^^ {
      case d => Rest(d)
    }

    def chordQuality: Parser[ChordQuality.Value] = """(min|.7|.6|.5|dim|aug|M7|m7|.sus4|.sus2|.9|M|m)?""".r ^^ {
      ChordQuality(_)
    }

    def chordName: Parser[Chord] = pitch ~ chordQuality ~ ":" ~ duration ^^ {
      case p ~ q ~ ":" ~ d => Chord(p, q, d)
    }

    def tempo: Parser[Int] = "tempo" ~> quantity ^^ {
      case t => t.toInt
    }

    def partial: Parser[Duration] = "partial" ~> duration ^^ {
      case p => p
    }

    def measure: Parser[Measure] = opt("|") ~> opt(timeSignature) ~ opt(key) ~ opt(clef) ~ opt(tempo) ~ opt(partial) ~ rep1(element) <~ opt("|") ^^ {
      case t ~ k ~ c ~ s ~ p ~ m => {
        val time = t.getOrElse(lastTime)
        val key = k.getOrElse(lastKey)
        val clef = c.getOrElse(lastClef)
        val tempo = s.getOrElse(lastTempo)
        val partial = p.getOrElse(null)
        if (lastTempo != tempo) startTempo
        val measure = Measure(time, key, clef, tempo, m.flatten, partial, lastTime != time, lastKey != key, lastClef != clef, (lastTempo != tempo) || startTempo)
        lastTime = time
        lastKey = key
        lastClef = clef
        lastTempo = tempo
        measure
      }
    }

    def repeat: Parser[Repeat] = "|:" ~> rep1(measure | repeat) ~ opt(rep1("[" ~> rep1(measure) <~ "]")) <~ ":|" ^^ {
      case m ~ None => new Repeat(m)
      case m ~ Some(a) => new Repeat(m, a)
    }

    def voice: Parser[Voice] = opt("(") ~> opt(instrument) ~ rep1(measure | repeat) <~ opt(")") ^^ {
      case None ~ m => new Voice(music = m)
      case Some(i) ~ m => new Voice(m, i)
    }

    def instrument: Parser[String] = "instr" ~> ("\'" ~> """([a-z,A-Z,0-9,(,), ]*)""".r) <~ "\'" ^^ {
      case i => i
    }

    def chords: Parser[ChordProgression] = "chords(" ~> opt(instrument) ~ rep1(measure | repeat) <~ ")" ^^ {
      case None ~ m => ChordProgression(music = m)
      case Some(i) ~ m => ChordProgression(m, i)
    }

    def key: Parser[Key] = "key" ~> pitch ~ opt("m") ^^ {
      case p ~ Some(m) => MinorScale(p)
      case p ~ None => MajorScale(p)
    }

    def clef: Parser[Clef.Value] = "clef" ~> """(treble|alto|tenor|bass)""".r ^^ {
      Clef(_)
    }

    def staff: Parser[Staff] = opt("(") ~> rep1(voice | chords) <~ opt(")") ^^ {
      case m => Staff(m)
    }

    def timeSignature: Parser[TimeSignature] = quantity ~ "/" ~ quantity ^^ {
      case d ~ "/" ~ n => TimeSignature(d.toInt, n.toInt)
    }

    def style: Parser[Style] = "style" ~> ("\'" ~> """([a-z,A-Z,0-9,(,), ]*)""".r) <~ "\'" ^^ {
      case s => s.toLowerCase() match {
        case "jazz" => Jazz
        case "rock" => Rock
        case "funk" => Funk
        case "slowrock" => SlowRock
        case "swing" => Swing
        case "reggae" => Reggae
        case "ska" => Ska
        case "boogie" => Boogie
        case "soul" => Soul
        case _ => println("No such style!"); null
      }
    }

    def score: Parser[Score] = opt("(") ~> opt(style) ~ rep1(staff) <~ opt(")") ^^ {
      case s ~ m => Score(s.getOrElse(null), m)
    }

    def apply(input: String): Score = parseAll(score, input) match {
      case Success(result, _) =>
        resetDefaults(); result
      case failure: NoSuccess => resetDefaults(); println("Input not valid!"); null //scala.sys.error(failure.msg)
    }

    def resetDefaults() {
      lastTime = TimeSignature()
      lastKey = MajorScale(Pitch())
      lastClef = Clef.treble
      lastTempo = 100
      setDefaultTempo = true
    }
  }

  implicit class DSLHelper(val sc: StringContext) extends AnyVal {
    def m(args: Any*) = {
      val score = DSLParser(sc.parts(0))
      if (score != null) score.asDSL
    }
    def show(args: Any*) = {
      val score = DSLParser(sc.parts(0))
      if (score != null) ShowAsLy(score)
    }
    def ly(args: Any*) = {
      val score = DSLParser(sc.parts(0))
      if (score != null) ShowAsLy.generateLy(score)
    }
    def generate(args: Any*) = {
      val score = DSLParser(sc.parts(0))
      if (score != null) ShowAsLy(new BasslineGenerator(DSLParser(sc.parts(0))).generate)
    }
  }

  object DSLGenerator {
    def apply(m: MusicConversion): String = m.asDSL
  }

  object ShowAsLy {

    def apply(m: MusicConversion) = {
      import java.io._
      import sys.process._

      val path = new File("./lilypond-output")
      if (!path.exists()) path.mkdir()
      val fileName = s"rc-${System.currentTimeMillis()}"
      val bw = new BufferedWriter(new FileWriter(path + "/" + fileName + ".ly"))
      bw.write(generateLy(m))
      bw.close()

      val resultLy = Process("lilypond --pdf " + fileName + ".ly", path).!!
      java.awt.Desktop.getDesktop.open(new File(path + "/" + fileName + ".mid"))
      java.awt.Desktop.getDesktop.open(new File(path + "/" + fileName + ".pdf"))
    }

    def generateLy(m: MusicConversion): String = {
      import java.util.Calendar
      s"""% ${Calendar.getInstance().getTime()}
      |
      |\\version "2.18.1"
      |
      |\\header { }
      |
      |\\layout { }
      |
      |\\paper { }
      |
      |${m.asLy}""".stripMargin
    }
  }
}

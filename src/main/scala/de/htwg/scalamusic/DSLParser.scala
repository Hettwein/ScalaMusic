package de.htwg.scalamusic

import scala.util.parsing.combinator.RegexParsers
import scala.io.Source

package object parser {

  object DSLParser extends RegexParsers {

    var lastTime: TimeSignature = TimeSignature()
    var lastKey: Key = MajorScale(Pitch())
    var lastClef: Clef.Value = Clef.treble
    var lastTempo: Int = 105

    def digit: Parser[String] = """(0|1|2|3|4|5|6|7|8|9)""".r ^^ {
      case d => d
    }

    def quantity: Parser[Int] = rep1(digit) ^^ {
      case q => q.foldLeft("")((x, y) => x + y).toInt
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
      case None ~ None ~ None ~ None ~ None ~ m => Measure(lastTime, lastKey, lastClef, lastTempo, m.flatten)
      case Some(t) ~ None ~ None ~ None ~ None ~ m =>
        lastTime = t; Measure(t, lastKey, lastClef, lastTempo, m.flatten, timeChange = true)
      case Some(t) ~ Some(k) ~ None ~ None ~ None ~ m =>
        lastTime = t; lastKey = k; Measure(t, k, lastClef, lastTempo, m.flatten, timeChange = true, keyChange = true)
      case Some(t) ~ None ~ Some(c) ~ None ~ None ~ m =>
        lastTime = t; lastClef = c; Measure(t, lastKey, c, lastTempo, m.flatten, timeChange = true, clefChange = true)
      case Some(t) ~ None ~ None ~ Some(s) ~ None ~ m =>
        lastTime = t; lastTempo = s; Measure(t, lastKey, lastClef, s, m.flatten, timeChange = true, tempoChange = true)
      case Some(t) ~ Some(k) ~ Some(c) ~ None ~ None ~ m =>
        lastTime = t; lastKey = k; lastClef = c; Measure(t, k, c, lastTempo, m.flatten, timeChange = true, keyChange = true, clefChange = true)
      case Some(t) ~ Some(k) ~ None ~ Some(s) ~ None ~ m =>
        lastTime = t; lastKey = k; lastTempo = s; Measure(t, k, lastClef, s, m.flatten, keyChange = true, tempoChange = true)
      case Some(t) ~ Some(k) ~ Some(c) ~ Some(s) ~ None ~ m =>
        lastTime = t; lastKey = k; lastClef = c; lastTempo = s; Measure(t, k, c, s, m.flatten, timeChange = true, keyChange = true, clefChange = true, tempoChange = true)
      case None ~ Some(k) ~ None ~ None ~ None ~ m =>
        lastKey = k; Measure(lastTime, k, lastClef, lastTempo, m.flatten, keyChange = true)
      case None ~ Some(k) ~ Some(c) ~ None ~ None ~ m =>
        lastKey = k; lastClef = c; Measure(lastTime, k, c, lastTempo, m.flatten, keyChange = true, clefChange = true)
      case None ~ Some(k) ~ None ~ Some(s) ~ None ~ m =>
        lastKey = k; lastTempo = s; Measure(lastTime, k, lastClef, s, m.flatten, keyChange = true, tempoChange = true)
      case None ~ Some(k) ~ Some(c) ~ Some(s) ~ None ~ m =>
        lastKey = k; lastClef = c; lastTempo = s; Measure(lastTime, k, c, s, m.flatten, keyChange = true, clefChange = true, tempoChange = true)
      case None ~ None ~ Some(c) ~ None ~ None ~ m =>
        lastClef = c; Measure(lastTime, lastKey, c, lastTempo, m.flatten, clefChange = true)
      case None ~ None ~ Some(c) ~ Some(s) ~ None ~ m =>
        lastClef = c; lastTempo = s; Measure(lastTime, lastKey, c, s, m.flatten, clefChange = true, tempoChange = true)
      case None ~ None ~ None ~ Some(s) ~ None ~ m =>
        lastTempo = s; Measure(lastTime, lastKey, lastClef, s, m.flatten, tempoChange = true)
      case None ~ None ~ None ~ None ~ Some(p) ~ m => Measure(lastTime, lastKey, lastClef, lastTempo, m.flatten, p)
      case Some(t) ~ None ~ None ~ None ~ Some(p) ~ m =>
        lastTime = t; Measure(t, lastKey, lastClef, lastTempo, m.flatten, p, timeChange = true)
      case Some(t) ~ Some(k) ~ None ~ None ~ Some(p) ~ m =>
        lastTime = t; lastKey = k; Measure(t, k, lastClef, lastTempo, m.flatten, p, timeChange = true, keyChange = true)
      case Some(t) ~ None ~ Some(c) ~ None ~ Some(p) ~ m =>
        lastTime = t; lastClef = c; Measure(t, lastKey, c, lastTempo, m.flatten, p, timeChange = true, clefChange = true)
      case Some(t) ~ None ~ None ~ Some(s) ~ Some(p) ~ m =>
        lastTime = t; lastTempo = s; Measure(t, lastKey, lastClef, s, m.flatten, p, timeChange = true, tempoChange = true)
      case Some(t) ~ Some(k) ~ Some(c) ~ None ~ Some(p) ~ m =>
        lastTime = t; lastKey = k; lastClef = c; Measure(t, k, c, lastTempo, m.flatten, p, timeChange = true, keyChange = true, clefChange = true)
      case Some(t) ~ Some(k) ~ None ~ Some(s) ~ Some(p) ~ m =>
        lastTime = t; lastKey = k; lastTempo = s; Measure(t, k, lastClef, s, m.flatten, p, timeChange = true, keyChange = true, tempoChange = true)
      case Some(t) ~ Some(k) ~ Some(c) ~ Some(s) ~ Some(p) ~ m =>
        lastTime = t; lastClef = c; lastTempo = s; Measure(t, k, c, s, m.flatten, p, timeChange = true, keyChange = true, clefChange = true, tempoChange = true)
      case None ~ Some(k) ~ None ~ None ~ Some(p) ~ m =>
        lastKey = k; Measure(lastTime, k, lastClef, lastTempo, m.flatten, p, keyChange = true)
      case None ~ Some(k) ~ Some(c) ~ None ~ Some(p) ~ m =>
        lastKey = k; lastClef = c; Measure(lastTime, k, c, lastTempo, m.flatten, p, keyChange = true, clefChange = true)
      case None ~ Some(k) ~ None ~ Some(s) ~ Some(p) ~ m =>
        lastKey = k; lastTempo = s; Measure(lastTime, k, lastClef, s, m.flatten, p, keyChange = true, tempoChange = true)
      case None ~ Some(k) ~ Some(c) ~ Some(s) ~ Some(p) ~ m =>
        lastKey = k; lastClef = c; lastTempo = s; Measure(lastTime, k, c, s, m.flatten, p, keyChange = true, clefChange = true, tempoChange = true)
      case None ~ None ~ Some(c) ~ None ~ Some(p) ~ m =>
        lastClef = c; Measure(lastTime, lastKey, c, lastTempo, m.flatten, p, clefChange = true)
      case None ~ None ~ Some(c) ~ Some(s) ~ Some(p) ~ m =>
        lastClef = c; lastTempo = s; Measure(lastTime, lastKey, c, s, m.flatten, p, clefChange = true, tempoChange = true)
      case None ~ None ~ None ~ Some(s) ~ Some(p) ~ m => lastTempo = s; Measure(lastTime, lastKey, lastClef, s, m.flatten, p, tempoChange = true)
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

    def style: Parser[Style] = "style" ~> (opt("\'") ~> """([a-z,A-Z,0-9,(,), ]*)""".r) <~ opt("\'") ^^ {
      case s => s match {
        case "jazz" => Jazz
        case "rock" => Rock
        case "funk" => Funk
        case "slowRock" => SlowRock
        case "swing" => Swing
        case "reggae" => Reggae
        case "boogie" => Boogie
        case "soul" => Soul
      }
    }

    def score: Parser[Score] = opt("(") ~> style ~ rep1(staff) <~ opt(")") ^^ {
      case s ~ m => Score(s, m)
    }

    def apply(input: String): Score = parseAll(score, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }

  }

  implicit class DSLHelper(val sc: StringContext) extends AnyVal {
    def m(args: Any*) = DSLParser(sc.parts(0)).asDSL
    def show(args: Any*) = ShowAsLy(DSLParser(sc.parts(0)))
    def ly(args: Any*) = ShowAsLy.generateLy(DSLParser(sc.parts(0)))
    def generate(args: Any*) = { ShowAsLy(new BassGenerator(DSLParser(sc.parts(0))).generate) }
  }

  object DSLGenerator {
    def apply(m: MusicConversion): String = m.asDSL
  }

  object ShowAsLy {

    def apply(m: MusicConversion) = {
      import java.io._
      import sys.process._

      val path = new File(getClass.getResource("").getPath).getParentFile.getParentFile.getParentFile.getParentFile.getParent + "/lilypond-output"
//      val fileName = s"rc-${System.currentTimeMillis()}"
//      val bw = new BufferedWriter(new FileWriter(path + "/" + fileName + ".ly"))
//      bw.write(generateLy(m))
//      bw.close()
//
//      val resultLy = Process("lilypond --pdf " + fileName + ".ly", new File(path)).!!
            //val path = new File(getClass.getResource("").getPath).getParentFile.getParentFile.getParentFile.getParentFile.getParent + "/lilypond-output"
            val fileName = s"rc-${System.currentTimeMillis()}"
            val bw = new BufferedWriter(new FileWriter( /*/path + "/" + */ fileName + ".ly"))
          		  bw.write(generateLy(m))
          		  bw.close()
          		  
          		  val resultLy = Process("lilypond --pdf " + fileName + ".ly" /*, new File(path)*/ ).!!
      //      println(resultLy)
      //      Process(fileName + ".mid", new File(path)).!!
      //      Process(fileName + ".pdf", new File(path)).!!
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

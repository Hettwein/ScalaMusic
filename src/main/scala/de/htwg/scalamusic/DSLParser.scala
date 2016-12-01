package de.htwg.scalamusic

import scala.util.parsing.combinator.RegexParsers

package object parser {

  object DSLParser extends RegexParsers {

    def pitchClass: Parser[PitchClass.Value] = """([a-g,A-G])""".r ^^ {
      PitchClass(_)
    }

    def pitchDecorator: Parser[PitchDecorator.Value] = """(isis|is|eses|ses|es|s|[n|#|x|X|\-|_]?)""".r ^^ {
      PitchDecorator(_)
    }

    def pitchOctave: Parser[Int] = """([,|']*)""".r ^^ {
      case s => s.count(_ == ''') - s.count(_ == ',')
    }

    def pitch: Parser[Pitch] = pitchClass ~ opt(pitchDecorator) ~ opt(pitchOctave) ^^ {
      case c ~ d ~ o => Pitch(c, d.getOrElse(PitchDecorator.Blank), o.getOrElse(0))
    }

    def noteAdditional: Parser[String] = """([.~]?)""".r ^^ {
      case s => s
    }

    def note: Parser[Note] = pitchClass ~ opt(pitchDecorator) ~
      opt(pitchOctave) ~ """([\d])""".r ~ opt(noteAdditional) ^^ {
        case c ~ d ~ o ~ b ~ Some(a) =>
          Note(Pitch(c, d.getOrElse(PitchDecorator.Blank),
            o.getOrElse(0)), if (a == ".") Beat(3, 2 * b.toInt) else Beat(1, b.toInt), a == "~")
      }

    def tuplet: Parser[Tuplet] = opt("(") ~> (rep1(note | chordName) <~ opt(")")) ~ """([\d])""".r ^^ {
      case m ~ d => Tuplet(d.toInt, m)//n.map { m => m.asInstanceOf[Note].copy(duration = Beat(1, m.duration.denominator / 2 * d.toInt)) }
    }

    def rest: Parser[Rest] = """([r,R])""".r ~ """([\d])""".r ^^ {
      case r ~ b => Rest(Beat(1, b.toInt))
    }

    def chordQuality: Parser[ChordQuality.Value] = """([m|min|M|7|6|dim|aug|M7|m7|sus4|sus2|9])""".r ^^ {
      ChordQuality(_)
    }

    def chordName: Parser[Chord] = pitch ~ chordQuality ~ ":" ~ """([\d]?)""".r ~ opt(noteAdditional) ^^ {
      case p ~ q ~ ":" ~ d ~ Some(a) => Chord(p, q, if (a == ".") Beat(3, 2 * d.toInt) else Beat(1, d.toInt), a == "~")
    }

//    def chord: Parser[Chord] = "<" ~ rep1(pitch) ~ ">" ~ """([\d]?)""".r ~ opt(noteAdditional) ^^ {
//      case "<" ~ p ~ ">" ~ d ~ Some(a) => Chord(p, if (a == ".") Beat(3, 2 * d.toInt) else Beat(1, d.toInt), 70)
//    }

    def tempo: Parser[Int] = "tempo" ~> """([\d]*)""".r ^^ {
      case t => t.toInt
    }

    def measure: Parser[Measure] = opt("|") ~> opt(timeSignature) ~ opt(key) ~ opt(clef) ~ opt(tempo) ~ rep1(note | rest | chordName | tuplet) <~ opt("|") ^^ {
      case None ~ None ~ None ~ None ~ m => Measure(music = m.flatten)
      case Some(t) ~ None ~ None ~ None ~ m => Measure(timeSignature = t, music = m.flatten, timeChange = true)
      case Some(t) ~ Some(k) ~ None ~ None ~ m => Measure(timeSignature = t, key = k, music = m.flatten, timeChange = true, keyChange = true)
      case Some(t) ~ None ~ Some(c) ~ None ~ m => Measure(timeSignature = t, clef = c, music = m.flatten, timeChange = true, clefChange = true)
      case Some(t) ~ None ~ None ~ Some(s) ~ m => Measure(timeSignature = t, tempo = s.toInt, music = m.flatten, timeChange = true, tempoChange = true)
      case Some(t) ~ Some(k) ~ Some(c) ~ None ~ m => Measure(timeSignature = t, key = k, clef = c, music = m.flatten, timeChange = true, keyChange = true, clefChange = true)
      case Some(t) ~ Some(k) ~ None ~ Some(s) ~ m => Measure(timeSignature = t, key = k, tempo = s.toInt, music = m.flatten, timeChange = true, keyChange = true, tempoChange = true)
      case Some(t) ~ Some(k) ~ Some(c) ~ Some(s) ~ m => Measure(t, k, c, s.toInt, m.flatten, true, true, true, true)
      case None ~ Some(k) ~ None ~ None ~ m => Measure(key = k, music = m.flatten, keyChange = true)
      case None ~ Some(k) ~ Some(c) ~ None ~ m => Measure(key = k, clef = c, music = m.flatten, keyChange = true, clefChange = true)
      case None ~ Some(k) ~ None ~ Some(s) ~ m => Measure(key = k, tempo = s.toInt, music = m.flatten, keyChange = true, tempoChange = true)
      case None ~ Some(k) ~ Some(c) ~ Some(s) ~ m => Measure(key = k, clef = c, tempo = s.toInt, music = m.flatten, keyChange = true, clefChange = true, tempoChange = true)
      case None ~ None ~ Some(c) ~ None ~ m => Measure(clef = c, music = m.flatten, clefChange = true)
      case None ~ None ~ Some(c) ~ Some(s) ~ m => Measure(clef = c, tempo = s.toInt, music = m.flatten, clefChange = true, tempoChange = true)
      case None ~ None ~ None ~ Some(s) ~ m => Measure(tempo = s.toInt, music = m.flatten, tempoChange = true)
    }

    def voice: Parser[Voice] = opt("(") ~> opt(instrument) ~ rep1(measure) <~ opt(")") ^^ {
      case None ~ m => new Voice(music = m)
      case Some(i) ~ m => new Voice(m, i)
    }

    def instrument: Parser[String] = "instr" ~> (opt("\'") ~> """([a-z,A-Z,0-9, ]*)""".r) <~ opt("\'") ^^ {
      case i => i
    }

    def chords: Parser[ChordProgression] = opt("chords(") ~> opt(instrument) ~ rep1(measure) <~ opt(")") ^^ {
      case None ~ m => ChordProgression(music = m)
      case Some(i) ~ m => ChordProgression(m, i)
    }

    def key: Parser[Mode] = "key" ~> pitch ~ opt("""([m])""".r) ^^ {
      case p ~ Some(m) => MinorScale(p)
      case p ~ None => MajorScale(p)
    }

    def clef: Parser[Clef.Value] = "clef" ~> """(treble|alto|tenor|bass)""".r ^^ {
      Clef(_)
    }

    def staff: Parser[Staff] = opt("(") ~> rep1(voice | chords) <~ opt(")") ^^ {
      case m => Staff(m)
    }

    def timeSignature: Parser[TimeSignature] = """(\d)""".r ~ "/" ~ """(\d)""".r ^^ {
      case d ~ "/" ~ n => TimeSignature(d.toInt, n.toInt)
    }

    def score: Parser[Score] = opt("(") ~> rep1(staff) <~ opt(")") ^^ {
      case m => Score(music = m)
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
    def generate(args: Any*) = new BassGenerator(DSLParser(sc.parts(0))).generate()
  }

  object DSLGenerator {
    def apply(m: MusicConversion): String = m.asDSL
  }

  object ShowAsLy {

    def apply(m: MusicConversion) = {
      import java.io._
      import sys.process._

      val fileName = s"\\rc-${System.currentTimeMillis()}"
      val bw = new BufferedWriter(new FileWriter(fileName + ".ly"))
      bw.write(generateLy(m))
      bw.close()

      val resultLy = Process("lilypond --pdf " + fileName + ".ly", new File("\\")).!!
      //      println(resultLy)
      //      s"open ${fileName}.pdf".!
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

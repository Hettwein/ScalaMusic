package de.htwg.scalamusic

import scala.util.parsing.combinator.RegexParsers
import scala.io.Source

package object parser {

  object DSLParser extends RegexParsers {

    def pitchClass: Parser[PitchClass.Value] = """([a-g,A-G])""".r ^^ {
      PitchClass(_)
    }

    def pitchDecorator: Parser[PitchDecorator.Value] = """(isis|is|eses|ses|es|s|[n|#|x|X|\-]?)""".r ^^ {
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

    //    def tiedNote: Parser[Note] = note ~ rep1("~" ~ note) ^^ {
    //      case n ~ tn => {
    //         val notes = tn.map(_._2)
    //         val sum = notes.foldLeft(Beat(0, 1))((n, m) => n.sum(m.duration))
    //        n.copy(duration = n.duration.sum(sum), tied = true)
    //      }
    //    }

    def tuplet: Parser[Tuplet] = opt("(") ~> (rep1(note | chordName) <~ opt(")")) ~ """([\d])""".r ^^ {
      case m ~ d => Tuplet(d.toInt, m)
    }

    def rest: Parser[Rest] = """([r,R])""".r ~ """([\d])""".r ^^ {
      case r ~ b => Rest(Beat(1, b.toInt))
    }

    def chordQuality: Parser[ChordQuality.Value] = """(min|.7|.6|.5|dim|aug|M7|m7|.sus4|.sus2|.9|M|m)?""".r ^^ {
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
    
    def partial: Parser[Int] = "partial" ~> """([\d]*)""".r ^^ {
      case p => p.toInt
    }

    def measure: Parser[Measure] = opt("|") ~> opt(timeSignature) ~ opt(key) ~ opt(clef) ~ opt(tempo) ~ opt(partial) ~ rep1(note | rest | chordName | tuplet) <~ opt("|") ^^ {
      case None ~ None ~ None ~ None ~ None ~ m => Measure(music = m.flatten)
      case Some(t) ~ None ~ None ~ None ~ None ~ m => Measure(timeSignature = t, music = m.flatten, timeChange = true)
      case Some(t) ~ Some(k) ~ None ~ None ~ None ~ m => Measure(timeSignature = t, key = k, music = m.flatten, timeChange = true, keyChange = true)
      case Some(t) ~ None ~ Some(c) ~ None ~ None ~ m => Measure(timeSignature = t, clef = c, music = m.flatten, timeChange = true, clefChange = true)
      case Some(t) ~ None ~ None ~ Some(s) ~ None ~ m => Measure(timeSignature = t, tempo = s.toInt, music = m.flatten, timeChange = true, tempoChange = true)
      case Some(t) ~ Some(k) ~ Some(c) ~ None ~ None ~ m => Measure(timeSignature = t, key = k, clef = c, music = m.flatten, timeChange = true, keyChange = true, clefChange = true)
      case Some(t) ~ Some(k) ~ None ~ Some(s) ~ None ~ m => Measure(timeSignature = t, key = k, tempo = s.toInt, music = m.flatten, timeChange = true, keyChange = true, tempoChange = true)
      case Some(t) ~ Some(k) ~ Some(c) ~ Some(s) ~ None ~ m => Measure(t, k, c, s.toInt, m.flatten, true, true, true, true)
      case None ~ Some(k) ~ None ~ None ~ None ~ m => Measure(key = k, music = m.flatten, keyChange = true)
      case None ~ Some(k) ~ Some(c) ~ None ~ None ~ m => Measure(key = k, clef = c, music = m.flatten, keyChange = true, clefChange = true)
      case None ~ Some(k) ~ None ~ Some(s) ~ None ~ m => Measure(key = k, tempo = s.toInt, music = m.flatten, keyChange = true, tempoChange = true)
      case None ~ Some(k) ~ Some(c) ~ Some(s) ~ None ~ m => Measure(key = k, clef = c, tempo = s.toInt, music = m.flatten, keyChange = true, clefChange = true, tempoChange = true)
      case None ~ None ~ Some(c) ~ None ~ None ~ m => Measure(clef = c, music = m.flatten, clefChange = true)
      case None ~ None ~ Some(c) ~ Some(s) ~ None ~ m => Measure(clef = c, tempo = s.toInt, music = m.flatten, clefChange = true, tempoChange = true)
      case None ~ None ~ None ~ Some(s) ~ None ~ m => Measure(tempo = s.toInt, music = m.flatten, tempoChange = true)
      case None ~ None ~ None ~ None ~ Some(p) ~ m => Measure(music = m.flatten, partial = p)
      case Some(t) ~ None ~ None ~ None ~ Some(p) ~ m => Measure(timeSignature = t, music = m.flatten, timeChange = true, partial = p)
      case Some(t) ~ Some(k) ~ None ~ None ~ Some(p) ~ m => Measure(timeSignature = t, key = k, music = m.flatten, timeChange = true, keyChange = true, partial = p)
      case Some(t) ~ None ~ Some(c) ~ None ~ Some(p) ~ m => Measure(timeSignature = t, clef = c, music = m.flatten, timeChange = true, clefChange = true, partial = p)
      case Some(t) ~ None ~ None ~ Some(s) ~ Some(p) ~ m => Measure(timeSignature = t, tempo = s.toInt, music = m.flatten, timeChange = true, tempoChange = true, partial = p)
      case Some(t) ~ Some(k) ~ Some(c) ~ None ~ Some(p) ~ m => Measure(timeSignature = t, key = k, clef = c, music = m.flatten, timeChange = true, keyChange = true, clefChange = true, partial = p)
      case Some(t) ~ Some(k) ~ None ~ Some(s) ~ Some(p) ~ m => Measure(timeSignature = t, key = k, tempo = s.toInt, music = m.flatten, timeChange = true, keyChange = true, tempoChange = true, partial = p)
      case Some(t) ~ Some(k) ~ Some(c) ~ Some(s) ~ Some(p) ~ m => Measure(t, k, c, s.toInt, m.flatten, true, true, true, true, partial = p)
      case None ~ Some(k) ~ None ~ None ~ Some(p) ~ m => Measure(key = k, music = m.flatten, keyChange = true, partial = p)
      case None ~ Some(k) ~ Some(c) ~ None ~ Some(p) ~ m => Measure(key = k, clef = c, music = m.flatten, keyChange = true, clefChange = true, partial = p)
      case None ~ Some(k) ~ None ~ Some(s) ~ Some(p) ~ m => Measure(key = k, tempo = s.toInt, music = m.flatten, keyChange = true, tempoChange = true, partial = p)
      case None ~ Some(k) ~ Some(c) ~ Some(s) ~ Some(p) ~ m => Measure(key = k, clef = c, tempo = s.toInt, music = m.flatten, keyChange = true, clefChange = true, tempoChange = true, partial = p)
      case None ~ None ~ Some(c) ~ None ~ Some(p) ~ m => Measure(clef = c, music = m.flatten, clefChange = true, partial = p)
      case None ~ None ~ Some(c) ~ Some(s) ~ Some(p) ~ m => Measure(clef = c, tempo = s.toInt, music = m.flatten, clefChange = true, tempoChange = true, partial = p)
      case None ~ None ~ None ~ Some(s) ~ Some(p) ~ m => Measure(tempo = s.toInt, music = m.flatten, tempoChange = true, partial = p)
    }

    def measures: Parser[Seq[Measure]] = rep1(measure) ^^ {
      case m => m
    }
    
    def repetition: Parser[Seq[Measure]] = "|:" ~> rep1(measures | repetition) ~ opt(rep1("[" ~> measures <~ "]"))<~ ":|" ^^ {
      case m ~ None => (m++m).flatten
      case m ~ Some(a) => (for(i <- 0 until a.size) yield m.flatten++a(i)).flatten
    }
    
    def voice: Parser[Voice] = opt("(") ~> opt(instrument) ~ rep1(measures | repetition) <~ opt(")") ^^ {
      case None ~ m => new Voice(music = m.flatten)
      case Some(i) ~ m => new Voice(m.flatten, i)
    }

    def instrument: Parser[String] = "instr" ~> (opt("\'") ~> """([a-z,A-Z,0-9,(,), ]*)""".r) <~ opt("\'") ^^ {
      case i => i
    }

    def chords: Parser[ChordProgression] = opt("chords(") ~> opt(instrument) ~ rep1(measures | repetition) <~ opt(")") ^^ {
      case None ~ m => ChordProgression(music = m.flatten)
      case Some(i) ~ m => ChordProgression(m.flatten, i)
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

    
    def style: Parser[String] = "style" ~> (opt("\'") ~> """([a-z,A-Z,0-9,(,), ]*)""".r) <~ opt("\'") ^^ {
      case s => s
    }
    
    def score: Parser[Score] = opt("(") ~> style ~ rep1(staff) <~ opt(")") ^^ {
      case s ~ m => Score(Style(s), m)
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
      val fileName = s"rc-${System.currentTimeMillis()}"
      val bw = new BufferedWriter(new FileWriter(path + "/" + fileName + ".ly"))
      bw.write(generateLy(m))
      bw.close()

      val resultLy = Process("lilypond --pdf " + fileName + ".ly", new File(path)).!!
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

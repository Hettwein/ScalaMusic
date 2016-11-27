package de.htwg.scalamusic.music

import scala.util.parsing.combinator.RegexParsers
import de.htwg.scalamusic.music.BassGenerator

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
            o.getOrElse(0)), if(a == ".") Beat(3, 2 * b.toInt) else Beat(1, b.toInt))
      }

    def rest: Parser[Rest] = """([r,R])""".r ~ """([\d])""".r ^^ {
      case r ~ b => Rest(Beat(1, b.toInt))
    }

    def chordQuality: Parser[ChordQuality.Value] = """([m|min|M|7|6|dim|aug|M7|m7|sus4|sus2|9])""".r ^^ {
      ChordQuality(_)
    }

    def chordName: Parser[Chord] = pitch ~ chordQuality ~ ":" ~ """([\d]?)""".r ~ opt(noteAdditional) ^^ {
      case p ~ q ~ ":" ~ d ~ Some(a) => Chord(p, q, if(a == ".") Beat(3, 2 * d.toInt) else Beat(1, d.toInt))
    }

    def chord: Parser[Chord] = "<" ~ rep1(pitch) ~ ">" ~ """([\d]?)""".r ~ opt(noteAdditional) ^^ {
      case "<" ~ p ~ ">" ~ d ~ Some(a) => Chord(p, if(a == ".") Beat(3, 2 * d.toInt) else Beat(1, d.toInt), 70)
    }

    def voice: Parser[Voice] = opt("(") ~> rep1(note | rest | chordName | chord) <~ opt(")") ^^ {
      case p => Voice(music = p)
    }

    def chords: Parser[ChordProgression] = opt("chords(") ~> rep1(chordName | chord) <~ opt(")") ^^ {
      case p => ChordProgression(music = p)
    }

    def key: Parser[Mode] = "key" ~> pitch ~ opt("""([m])""".r) ^^ {
      case p ~ Some(m) => MinorScale(p)
      case p ~ None => MajorScale(p)
    }

    def clef: Parser[Clef.Value] = "clef" ~> """(treble|alto|tenor|bass)""".r ^^ {
      Clef(_)
    }

    def staff: Parser[Staff] = opt("(") ~> opt(clef) ~ opt(key) ~ rep1(voice | chords) <~ opt(")") ^^ {
      case None ~ None ~ m => Staff(music = m)
      case Some(c) ~ None ~ m => Staff(clef = c, music = m)
      case None ~ Some(k) ~ m => Staff(key = k, music = m)
      case Some(c) ~ Some(k) ~ m => Staff(c, k, m)
    }

    def timeSignature: Parser[TimeSignature] = """(\d)""".r ~ "/" ~ """(\d)""".r ^^ {
      case d ~ "/" ~ n => TimeSignature(d.toInt, n.toInt)
    }

    def score: Parser[Score] = opt("(") ~> opt(timeSignature) ~ rep1(staff) <~ opt(")") ^^ {
      case None ~ m => Score(music = m)
      case Some(t) ~ m => Score(105, t, m)
    }

//    def music: Parser[MusicDSL] = score// | chords | voice | chord | note | rest | pitch // | score

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

      val fileName = s"\\HTWG\\Bachelorarbeit\\ScalaMusic\\rc-${System.currentTimeMillis()}"
      //      \HTWG\Bachelorarbeit\scala-music-dsl
      val bw = new BufferedWriter(new FileWriter(fileName + ".ly"))
      bw.write(generateLy(m))
      bw.close()

      val resultLy = Process("lilypond --pdf " + fileName + ".ly", new File("\\HTWG\\Bachelorarbeit\\ScalaMusic")).!!
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

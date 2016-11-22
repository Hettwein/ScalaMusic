package de.htwg.scalamusic.music

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
        case c ~ d ~ o ~ b ~ a =>
          Note(Pitch(c, d.getOrElse(PitchDecorator.Blank),
            o.getOrElse(0)), Beat(1, b.toInt))
      }

    def rest: Parser[Rest] = """([r,R])""".r ~ """([\d])""".r ^^ {
      case r ~ b => Rest(Beat(1, b.toInt))
    }

    def chordQuality: Parser[ChordQuality.Value] = """([m|min|M|7|6|dim|aug|M7|m7|sus4|sus2|9])""".r ^^ {
      ChordQuality(_)
    }

    def chord: Parser[Chord] = pitch ~ chordQuality ~ """([\d]?)""".r ^^ {
      case p ~ q ~ d => Chord(p, q, Beat(1, d.toInt))
    }

    def voice: Parser[Voice] = opt("voice{") ~> rep1(note | rest | chord) <~ opt("}") ^^ {
      case p => Voice(music = p)
    }

    def chords: Parser[ChordProgression] = opt("chords{") ~> rep1(chord) <~ opt("}") ^^ {
      case p => ChordProgression(music = p)
    }

    //    def timeSig: Parser[TimeSignature] = """(\[\d,\d\])""".r ^^ {
    //      case n => TimeSignature(n.)
    //    }

    def key: Parser[Mode] = "key" ~> pitch ~ opt("""([m])""".r) ^^ {
      case p ~ Some(m) => MinorScale(p)
      case p ~ None => MajorScale(p)
    }
    
    def clef: Parser[Clef.Value] = "clef" ~> """(treble|alto|tenor|bass)""".r ^^ {
      Clef(_)
    }
    
    def staff: Parser[Staff] = opt("staff<<") ~> opt(clef) ~ opt(key) ~ rep1(voice | chords) <~ opt(">>") ^^ {
      case None ~ None ~ m => Staff(music = m)
      case Some(c) ~ None ~ m => Staff(clef = c, music = m)
      case None ~ Some(k) ~ m => Staff(key = k, music = m)
      case Some(c) ~ Some(k) ~ m => Staff(c, k, m)
    }

    def score: Parser[Score] = opt("score<<") ~> rep1(staff) <~ opt(">>") ^^ {
      case s => Score(music = s)
    }

    def music: Parser[MusicDSL] = score | chords | voice | chord | note | rest | pitch // | score

    def apply(input: String): MusicDSL = parseAll(music, input) match {
      case Success(result, _) => result
      case failure: NoSuccess => scala.sys.error(failure.msg)
    }

  }

  implicit class DSLHelper(val sc: StringContext) extends AnyVal {
    def m(args: Any*) = DSLParser(sc.parts(0))
    def show(args: Any*) = ShowAsLy(DSLParser(sc.parts(0)))
    def ly(args: Any*) = ShowAsLy.generateLy(DSLParser(sc.parts(0)))
  }

  object DSLGenerator {
    def apply(m: MusicDSL): String = m.asDSL
  }

  object ShowAsLy {

    def apply(m: MusicDSL) = {
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

    def generateLy(m: MusicDSL): String = {
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
    //    def generateLy(m: MusicDSL): String = {
    //    		import java.util.Calendar
    //    		s"""% ${Calendar.getInstance().getTime()}
    //    		|
    //    		|\\version "2.18.1"
    //    		|
    //    		|\\header { }
    //    		|
    //    		|\\layout { }
    //    		|
    //    		|\\paper { }
    //    		|
    //    		|\\score {
    //    		|  \\new Staff {
    //    		|    ${m.asLy}
    //    		|  }
    //    		|}""".stripMargin
    //    }

  }
}

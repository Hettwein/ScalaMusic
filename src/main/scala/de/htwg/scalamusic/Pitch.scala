package de.htwg.scalamusic

import scala.language.postfixOps

case class Pitch(pitchClass: PitchClass.Value = PitchClass.C, decorator: PitchDecorator.Value = PitchDecorator.Blank, octave: Int = 0) extends MusicConversion {

  def toPitchNumber: Int = {
    PitchClass.toPitchNumber(pitchClass) + PitchDecorator.toPitchNumber(decorator) + (octave * 12)
  }

  def + = copy(octave = octave + 1)
  def - = copy(octave = octave - 1)

  override def toString: String = {
    var octaves = "";
    if (octave >= 0) {
      octaves = "'" * octave
    } else {
      octaves = "," * Math.abs(octave)
    }

    pitchClass.toString.toLowerCase + PitchDecorator.toString(decorator) + octaves
  }

  def asLy: String = toString
  def asDSL: String = toString
}

object PitchClass extends Enumeration {
  type PitchClass = Value
  val C, D, E, F, G, A, B = Value

  private val midi = Map(
    C -> 0,
    D -> 2,
    E -> 4,
    F -> 5,
    G -> 7,
    A -> 9,
    B -> 11
  )

  def apply(s: String): PitchClass = withName(s.toUpperCase)
  def toPitchNumber(p: PitchClass): Int = midi(p)
}

object PitchDecorator extends Enumeration {
  type PitchDecorator = Value
  val Blank, Natural, Sharp, Flat, DoubleSharp, DoubleFlat = Value

  private val decStr = Map(
    Blank -> "",
    Natural -> "",
    Sharp -> "is",
    Flat -> "es",
    DoubleSharp -> "isis",
    DoubleFlat -> "eses"
  )

  private val dec = Map(
    "n" -> Natural,
    "is" -> Sharp,
    "es" -> Flat,
    "s" -> Flat,
    "isis" -> DoubleSharp,
    "eses" -> DoubleFlat,
    "ses" -> DoubleFlat
  )

  private val midi = Map(
    Sharp -> 1,
    Flat -> -1,
    DoubleSharp -> 2,
    DoubleFlat -> -2
  )

  def apply(s: String): PitchDecorator = dec.getOrElse(s, Blank)
  def toPitchNumber(d: PitchDecorator): Int = midi.getOrElse(d, 0)
  def toString(d: PitchDecorator.Value): String = decStr(d)
}

object Pitch {
  import PitchClass._
  import PitchDecorator._
  import KeySignatureSpelling._

  private val r = """([a-g,A-G])(isis|is|eses|ses|es|s|)([,|']*)""".r

  private def midi(d: (Int, KeySignatureSpelling.Value, Int)) = d match{
    case (0, _, _) => if(d._2 == Sharps && d._3 > 6) "Bis" else "C"
    case (1, _, _) => if(d._2 == Sharps) "Cis" else "Des"
    case (2, _, _) => "D"
    case (3, _, _) => if(d._2 == Sharps) "Dis" else "Es"
    case (4, _, _) => if(d._2 == Flats && d._3 > 6) "Fes" else "E"
    case (5, _, _) => if(d._2 == Sharps && d._3 > 5) "Eis" else "F"
    case (6, _, _) => if(d._2 == Sharps) "Fis" else "Ges"
    case (7, _, _) => "G"
    case (8, _, _) => if(d._2 == Sharps) "Gis" else "As"
    case (9, _, _) => "A"
    case (10, _, _) => if(d._2 == Sharps) "Ais" else "Bes"
    case (11, _, _) => if(d._2 == Flats && d._3 > 5) "Ces" else "B"
  }

  def apply(s: String): Pitch = s match {
    case r(p) => new Pitch(PitchClass(p), Blank, 0)
    case r(p, d) => new Pitch(PitchClass(p), PitchDecorator(d), 0)
    case r(p, d, o) => new Pitch(PitchClass(p), PitchDecorator(d),
      o.count(c => (c == ''')) - o.count(c => (c == ',')))
  }

  def apply(i: Int, signs: (KeySignatureSpelling.Value, Int)): Pitch = {
    var octaves = "";
    if (i >= 0) {
      octaves = "'" * (i / 12)
    } else {
      octaves = "," * (Math.abs(i - 12) / 12)
    }
    Pitch((midi(Math.abs((i + 48) % 12), signs._1, signs._2) + octaves))
  }
}
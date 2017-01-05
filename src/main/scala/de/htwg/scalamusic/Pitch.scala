package de.htwg.scalamusic

import scala.language.postfixOps

case class Pitch(pitchClass: PitchClass.Value = PitchClass.C, decorator: PitchDecorator.Value = PitchDecorator.Blank, octave: Int = 0) extends MusicConversion {

  def toPitchNumber: Int = {
    PitchClass.toPitchNumber(pitchClass) + PitchDecorator.toPitchNumber(decorator) + (octave * 12)
  }

  def + = copy(octave = octave + 1)
  def - = copy(octave = octave - 1)

//  def chromaticUp(sign: KeySignatureSpelling.Value) = Pitch((Pitch.midi(this.toPitchNumber + 1, sign)))
//  def chromaticDown(sign: KeySignatureSpelling.Value) = Pitch((Pitch.midi(this.toPitchNumber - 1, sign)))

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
  def isIn(p: Int): Boolean = midi.map(_.swap).get(p) != None
  def getClass(p: Int): PitchClass.Value = midi.map(_.swap).get(p).get
  //  def valueStream = Stream.continually(PitchClass.values).flatten
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

  private val midi = Map( //
    (0, Sharps) -> "Bis",
    (0, Sharps) -> "C",
    (0, Flats) -> "C",
    (1, Sharps) -> "Cis",
    (1, Flats) -> "Des",
    (2, Sharps) -> "D",
    (2, Flats) -> "D",
    (3, Sharps) -> "Dis",
    (3, Flats) -> "Es",
    (4, Sharps) -> "E",
    (4, Flats) -> "E",
    (4, Flats) -> "Fes",
    (5, Sharps) -> "Eis",
    (5, Sharps) -> "F",
    (5, Flats) -> "F",
    (6, Sharps) -> "Fis",
    (6, Flats) -> "Ges",
    (7, Sharps) -> "G",
    (7, Flats) -> "G",
    (8, Sharps) -> "Gis",
    (8, Flats) -> "As",
    (9, Sharps) -> "A",
    (9, Flats) -> "A",
    (10, Sharps) -> "Ais",
    (10, Flats) -> "Bes",
    (11, Sharps) -> "B",
    (11, Flats) -> "B",
    (11, Flats) -> "Ces"
  )

  def apply(s: String): Pitch = s match {
    case r(p) => new Pitch(PitchClass(p), Blank, 0)
    case r(p, d) => new Pitch(PitchClass(p), PitchDecorator(d), 0)
    case r(p, d, o) => new Pitch(PitchClass(p), PitchDecorator(d),
      o.count(c => (c == ''')) - o.count(c => (c == ',')))
  }

  def apply(i: Int, sign: KeySignatureSpelling.Value): Pitch = {
    var octaves = "";
    if (i >= 0) {
      octaves = "'" * (i / 12)
    } else {
      octaves = "," * (Math.abs(i - 12) / 12)
    }
    Pitch((midi(Math.abs((i + 48) % 12), sign) + octaves))
  }

  //  def apply(pitch: PitchClass.Value): Pitch = new Pitch(pitch, Blank, 0)
}
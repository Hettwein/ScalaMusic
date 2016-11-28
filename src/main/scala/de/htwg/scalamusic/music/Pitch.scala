package de.htwg.scalamusic.music

case class Pitch(pitchClass: PitchClass.Value = PitchClass.C, decorator: PitchDecorator.Value = PitchDecorator.Blank, octave: Int = 0) extends MusicConversion {

  def toPitchNumber: Int = {
    PitchClass.toPitchNumber(pitchClass) + PitchDecorator.toPitchNumber(decorator) + (octave * 12)
  }

  override def toString: String = {
    var octaves = "";
    if (octave >= 0) { octaves = "'" * octave }
    else { octaves = "," * Math.abs(octave) }

    pitchClass.toString + PitchDecorator.toString(decorator) + octaves
  }
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
  def toPitchNumber(p: PitchClass): Int = midi.getOrElse(p, 0)
  def isIn(p: Int): Boolean = midi.map(_.swap).get(p) != None
  def getClass(p: Int): PitchClass.Value = midi.map(_.swap).get(p).get
  //  def valueStream = Stream.continually(PitchClass.values).flatten
}

object PitchDecorator extends Enumeration {
  type PitchDecorator = Value
  val Blank, Natural, Sharp, Flat, DoubleSharp, DoubleFlat = Value

  private val decStr = Map(
    Natural -> "",
    Sharp -> "is",
    Flat -> "es",
    DoubleSharp -> "isis",
    DoubleFlat -> "eses"
  )

  private val dec = Map(
    "n" -> Natural,
    "is" -> Sharp,
    "#" -> Sharp,
    "es" -> Flat,
    "s" -> Flat,
    "-" -> Flat,
    "isis" -> DoubleSharp,
    "x" -> DoubleSharp,
    "##" -> DoubleSharp,
    "eses" -> DoubleFlat,
    "ses" -> DoubleFlat,
    "_" -> DoubleFlat
  )

  private val midi = Map(
    Sharp -> 1,
    Flat -> -1,
    DoubleSharp -> 2,
    DoubleFlat -> -2
  )

  def apply(s: String): PitchDecorator = dec.getOrElse(s, Blank)
  def toPitchNumber(d: PitchDecorator): Int = midi.getOrElse(d, 0)
  //  def toString(d: PitchDecorator.Value): String = dec.map(_.swap).getOrElse(d, "")
  def toString(d: PitchDecorator.Value): String = decStr.getOrElse(d, "")
}

object Pitch {
  import PitchClass._
  import PitchDecorator._

  private val r = """([a-g,A-G])([isis|eses|ses|is|es|s|n|#|x|X|\-|_]?)([,|']*)""".r

  private val midi = Map(
    0 -> "C",
    1 -> "C#",
    2 -> "D",
    3 -> "E-",
    4 -> "E",
    5 -> "F",
    6 -> "F#",
    7 -> "G",
    8 -> "A-",
    9 -> "A",
    10 -> "B-",
    11 -> "B"
  )

  private val sharpMidi = Map(
    0 -> "C",
    1 -> "C#",
    2 -> "D",
    3 -> "D#",
    4 -> "E",
    5 -> "F",
    6 -> "F#",
    7 -> "G",
    8 -> "G#",
    9 -> "A",
    10 -> "A#",
    11 -> "B"
  )

  //  private val toMidi = Map(
  //    "bis" -> 0,
  //    "c" -> 0,
  //    "cis" -> 1,
  //    "des" -> 1,
  //    "d" -> 2,
  //    "dis" -> 3,
  //    "es" -> 3,
  //    "ees" -> 3,
  //    "e" -> 4,
  //    "fes" -> 4,
  //    "eis" -> 5,
  //    "f" -> 5,
  //    "fis" -> 6,
  //    "ges" -> 6,
  //    "g" -> 7,
  //    "gis" -> 8,
  //    "as" -> 8,
  //    "aes" -> 8,
  //    "a" -> 9,
  //    "ais" -> 10,
  //    "bes" -> 10,
  //    "b" -> 11,
  //    "ces" -> 11
  //  )

  def apply(s: String): Pitch = s match {
    case r(p) => new Pitch(PitchClass(p), Blank, 0)
    case r(p, d) => new Pitch(PitchClass(p), PitchDecorator(d), 0)
    case r(p, d, o) => new Pitch(PitchClass(p), PitchDecorator(d),
      o.count(c => (c == ''')) - o.count(c => (c == ',')))
    case _ => ???
  }

  def apply(i: Int): Pitch = {
    var octaves = "";
    if (i >= 0) {
      octaves = "'" * (i / 12)
    } else {
      octaves = "," * (Math.abs(i - 12) / 12)
    }
    Pitch((midi.getOrElse(Math.abs((i + 48) % 12), "C") + octaves))
  }

  //  def apply(pitch: PitchClass.Value): Pitch = new Pitch(pitch, Blank, 0)
}
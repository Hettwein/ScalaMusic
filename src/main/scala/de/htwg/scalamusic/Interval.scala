package de.htwg.scalamusic

case class Interval(root: Pitch = Pitch(), quality: IntervalQuality.Value, duration: Beat = Beat(), override val tied: Boolean = false, velocity: Int = 70) extends MusicElement {

  val music = quality match {
    case _ => Seq(root)
    //    case IntervalQuality. => Seq(root)
  }

  def asLy: String = toString
  def asDSL: String = toString
}

object Interval {
  def apply(duration: Beat, velocity: Int, music: Pitch*) {

  }

  def getPitch(from: Pitch, quality: IntervalQuality.Value, signs: KeySignatureSpelling.Value): Pitch = {
    //    def pitchClass = PitchClass.valueStream.dropWhile(_ != from.pitchClass)(number - 1)
    var pitchClass = from.toPitchNumber + IntervalQuality.toPitchNumber(quality)
    var octave = from.octave
    pitchClass -= 12 * octave
    while (pitchClass > 11) {
      pitchClass -= 12
      octave += 1
    }
    while (pitchClass < 0) {
      pitchClass += 12
      octave -= 1 //?
    }

    if (PitchClass.isIn(pitchClass)) {
      Pitch(PitchClass.getClass(pitchClass), PitchDecorator.Blank, octave)
    } else if (signs == KeySignatureSpelling.Sharps) {
      Pitch(PitchClass.getClass(pitchClass - 1), PitchDecorator.Sharp, octave)
    } else {
      Pitch(PitchClass.getClass(pitchClass + 1), PitchDecorator.Flat, octave)
    }
  }
}

object IntervalQuality extends Enumeration {
  type IntervalQuality = Value
  val Unison, MinorSecond, MajorSecond, MinorThird, MajorThird, Fourth, Tritone, Fifth, MinorSixth, MajorSixth, MinorSeventh, MajorSeventh, Octave, Ninth = Value
  private val midi = Map(
    Unison -> 0,
    MinorSecond -> 1,
    MajorSecond -> 2,
    MinorThird -> 3,
    MajorThird -> 4,
    Fourth -> 5,
    Tritone -> 6,
    Fifth -> 7,
    MinorSixth -> 8,
    MajorSixth -> 9,
    MinorSeventh -> 10,
    MajorSeventh -> 11,
    Octave -> 12,
    Ninth -> 14)
  def toPitchNumber(quality: IntervalQuality.Value): Int = midi(quality)
}
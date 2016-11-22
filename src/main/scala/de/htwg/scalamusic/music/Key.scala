package de.htwg.scalamusic.music

object ScaleDegree extends Enumeration {
  type ScaleDegree = Value
  val I, II, III, IV, V, VI, VII = Value
}

object KeySignatureSpelling extends Enumeration {
  type KeySignatureSpelling = Value
  val Mixed, Sharps, Flats = Value
}

trait HasKeySignatureSpelling {
  def getSpelling: KeySignatureSpelling.Value
}

trait Mode extends MusicDSL with HasKeySignatureSpelling {
  def getDegreePitch(d: ScaleDegree.Value): Pitch
}

case class MajorScale(root: Pitch) extends Mode {
  import KeySignatureSpelling._

  def getSpelling: KeySignatureSpelling.Value = root match {
    case Pitch(_, PitchDecorator.Sharp, _) => Sharps
    case Pitch(_, PitchDecorator.Flat, _) => Flats
    case Pitch(PitchClass.C, _, _) => Sharps
    case Pitch(PitchClass.G, _, _) => Sharps
    case Pitch(PitchClass.D, _, _) => Sharps
    case Pitch(PitchClass.A, _, _) => Sharps
    case Pitch(PitchClass.E, _, _) => Sharps
    case Pitch(PitchClass.B, _, _) => Sharps
    case Pitch(PitchClass.F, _, _) => Flats
  }

  def getDegreePitch(d: ScaleDegree.Value): Pitch = d match {
    case ScaleDegree.I => root
    case ScaleDegree.II => Interval.getPitch(root, IntervalQuality.MajorSecond, getSpelling)
    case ScaleDegree.III => Interval.getPitch(root, IntervalQuality.MajorThird, getSpelling)
    case ScaleDegree.IV => Interval.getPitch(root, IntervalQuality.Fourth, getSpelling)
    case ScaleDegree.V => Interval.getPitch(root, IntervalQuality.Fifth, getSpelling)
    case ScaleDegree.VI => Interval.getPitch(root, IntervalQuality.MajorSixth, getSpelling)
    case ScaleDegree.VII => Interval.getPitch(root, IntervalQuality.MajorSeventh, getSpelling)
  }

  override def asLy: String = s"""\\key ${root.asLy} \\major"""
  override def asDSL: String = s"""<< >>"""
}

case class MinorScale(root: Pitch) extends Mode {
	import KeySignatureSpelling._
	
	def getSpelling: KeySignatureSpelling.Value = root match {
	case Pitch(_, PitchDecorator.Sharp, _) => Sharps
	case Pitch(_, PitchDecorator.Flat, _) => Flats
	case Pitch(PitchClass.A, _, _) => Sharps
	case Pitch(PitchClass.E, _, _) => Sharps
	case Pitch(PitchClass.B, _, _) => Sharps
	case Pitch(PitchClass.F, _, _) => Flats
	case Pitch(PitchClass.D, _, _) => Flats
	case Pitch(PitchClass.C, _, _) => Flats
	case Pitch(PitchClass.G, _, _) => Flats
}

def getDegreePitch(d: ScaleDegree.Value): Pitch = d match {
case ScaleDegree.I => root
case ScaleDegree.II => Interval.getPitch(root, IntervalQuality.MajorSecond, getSpelling)
case ScaleDegree.III => Interval.getPitch(root, IntervalQuality.MinorThird, getSpelling)
case ScaleDegree.IV => Interval.getPitch(root, IntervalQuality.Fourth, getSpelling)
case ScaleDegree.V => Interval.getPitch(root, IntervalQuality.Fifth, getSpelling)
case ScaleDegree.VI => Interval.getPitch(root, IntervalQuality.MinorSixth, getSpelling)
case ScaleDegree.VII => Interval.getPitch(root, IntervalQuality.MinorSeventh, getSpelling)
}

override def asLy: String = s"""\\key ${root.asLy} \\minor"""
override def asDSL: String = s"""<< >>"""
}
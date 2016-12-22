package de.htwg.scalamusic

import scala.language.postfixOps

object ScaleDegree extends Enumeration {
  type ScaleDegree = Value
  val I, II, III, IIIb, IV, V, Vb, VI, VII, VIIb, VIII = Value
}

object KeySignatureSpelling extends Enumeration {
  type KeySignatureSpelling = Value
  val Mixed, Sharps, Flats = Value
}

trait HasKeySignatureSpelling {
  def getSpelling: KeySignatureSpelling.Value
}

trait Key extends MusicConversion with HasKeySignatureSpelling {
  def getDegreePitch(d: ScaleDegree.Value): Pitch

  val scale: Seq[Pitch] = ScaleDegree.values.toSeq.map { x => getDegreePitch(x) }

//  val degree = Map(
//    scale(0) -> ScaleDegree.I,
//    scale(1) -> ScaleDegree.II,
//    scale(2) -> ScaleDegree.III,
//    scale(3) -> ScaleDegree.IV,
//    scale(4) -> ScaleDegree.V,
//    scale(5) -> ScaleDegree.VI,
//    scale(6) -> ScaleDegree.VII,
//    scale(7) -> ScaleDegree.VIII
//  )

  def getDegreeIndex(p: Pitch): Int = { val idx = scale.indexOf(p); if (idx != -1) idx else if (getSpelling == KeySignatureSpelling.Flats) scale.indexOf(p.chromaticUp(getSpelling)) else scale.indexOf(p.chromaticDown(getSpelling)) }
  def stepUp(from: Pitch): Pitch = { val idx = scale.indexOf(from); if (idx < 6) scale(idx + 1) else scale(idx - 6)+ }
  def stepDown(from: Pitch): Pitch = { val idx = scale.indexOf(from); if (idx > 0) scale(idx - 1) else scale(idx + 6)- }
}

case class MajorScale(root: Pitch) extends Key {
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
    case ScaleDegree.IIIb => Interval.getPitch(root, IntervalQuality.MinorThird, KeySignatureSpelling.Flats)
    case ScaleDegree.IV => Interval.getPitch(root, IntervalQuality.Fourth, getSpelling)
    case ScaleDegree.V => Interval.getPitch(root, IntervalQuality.Fifth, getSpelling)
    case ScaleDegree.Vb => Interval.getPitch(root, IntervalQuality.Tritone, KeySignatureSpelling.Flats)
    case ScaleDegree.VI => Interval.getPitch(root, IntervalQuality.MajorSixth, getSpelling)
    case ScaleDegree.VII => Interval.getPitch(root, IntervalQuality.MajorSeventh, getSpelling)
    case ScaleDegree.VIIb => Interval.getPitch(root, IntervalQuality.MinorSeventh, KeySignatureSpelling.Flats)
    case ScaleDegree.VIII => Interval.getPitch(root, IntervalQuality.Octave, getSpelling)
  }

  override def asLy: String = s"""\\key ${root.asLy} \\major """
  override def asDSL: String = s"""key ${root.asDSL} """
}

case class MinorScale(root: Pitch) extends Key {
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
    case ScaleDegree.IIIb => Interval.getPitch(root, IntervalQuality.MinorThird, KeySignatureSpelling.Flats)
    case ScaleDegree.IV => Interval.getPitch(root, IntervalQuality.Fourth, getSpelling)
    case ScaleDegree.V => Interval.getPitch(root, IntervalQuality.Fifth, getSpelling)
    case ScaleDegree.Vb => Interval.getPitch(root, IntervalQuality.Tritone, KeySignatureSpelling.Flats)
    case ScaleDegree.VI => Interval.getPitch(root, IntervalQuality.MinorSixth, getSpelling)
    case ScaleDegree.VII => Interval.getPitch(root, IntervalQuality.MinorSeventh, getSpelling)
    case ScaleDegree.VIIb => Interval.getPitch(root, IntervalQuality.MinorSeventh, KeySignatureSpelling.Flats)
    case ScaleDegree.VIII => Interval.getPitch(root, IntervalQuality.Octave, getSpelling)
  }

  override def asLy: String = s"""\\key ${root.asLy} \\minor """
  override def asDSL: String = s"""key ${root.asDSL}m """
}

case class MinorPentatonicScale(root: Pitch) extends Key {
  import KeySignatureSpelling._

  def getDegreePitch(d: ScaleDegree.Value): Pitch = d match {
    case ScaleDegree.I => root
    case ScaleDegree.II => Interval.getPitch(root, IntervalQuality.MinorThird, getSpelling)
    case ScaleDegree.III => Interval.getPitch(root, IntervalQuality.Fourth, getSpelling)
    case ScaleDegree.IV => Interval.getPitch(root, IntervalQuality.Fifth, getSpelling)
    case ScaleDegree.V => Interval.getPitch(root, IntervalQuality.MinorSeventh, getSpelling)
    case ScaleDegree.VI => Interval.getPitch(root, IntervalQuality.Octave, getSpelling)
    case _ => root
  }

  def getSpelling: KeySignatureSpelling.Value = root match {
    case Pitch(_, PitchDecorator.Sharp, _) => Sharps
    case Pitch(_, PitchDecorator.Flat, _) => Flats
    case Pitch(PitchClass.C, _, _) => Flats
    case Pitch(PitchClass.G, _, _) => Sharps
    case Pitch(PitchClass.D, _, _) => Sharps
    case Pitch(PitchClass.A, _, _) => Sharps
    case Pitch(PitchClass.E, _, _) => Sharps
    case Pitch(PitchClass.B, _, _) => Sharps
    case Pitch(PitchClass.F, _, _) => Flats
  }

  override def asLy: String = s"""\\key ${root.asLy} \\major """
  override def asDSL: String = s"""key ${root.asDSL} """
}

case class MixolydianScale(root: Pitch) extends Key {
  import KeySignatureSpelling._

  def getSpelling: KeySignatureSpelling.Value = root match {
    case Pitch(_, PitchDecorator.Sharp, _) => Sharps
    case Pitch(_, PitchDecorator.Flat, _) => Flats
    case Pitch(PitchClass.G, _, _) => Sharps
    case Pitch(PitchClass.D, _, _) => Sharps
    case Pitch(PitchClass.A, _, _) => Sharps
    case Pitch(PitchClass.E, _, _) => Sharps
    case Pitch(PitchClass.B, _, _) => Sharps
    case Pitch(PitchClass.C, _, _) => Flats
    case Pitch(PitchClass.F, _, _) => Flats
  }

  def getDegreePitch(d: ScaleDegree.Value): Pitch = d match {
    case ScaleDegree.I => root
    case ScaleDegree.II => Interval.getPitch(root, IntervalQuality.MajorSecond, getSpelling)
    case ScaleDegree.III => Interval.getPitch(root, IntervalQuality.MajorThird, getSpelling)
    case ScaleDegree.IV => Interval.getPitch(root, IntervalQuality.Fourth, getSpelling)
    case ScaleDegree.V => Interval.getPitch(root, IntervalQuality.Fifth, getSpelling)
    case ScaleDegree.VI => Interval.getPitch(root, IntervalQuality.MajorSixth, getSpelling)
    case ScaleDegree.VII => Interval.getPitch(root, IntervalQuality.MinorSeventh, getSpelling)
    case ScaleDegree.VIII => Interval.getPitch(root, IntervalQuality.Octave, getSpelling)
  }

  override def asLy: String = s"""\\key ${getDegreePitch(ScaleDegree.IV).asLy} \\major """
  override def asDSL: String = s"""key ${getDegreePitch(ScaleDegree.IV).asDSL} """
}

//case class LocrianScale(root: Pitch) extends Key {
//  import KeySignatureSpelling._
//
//  def getSpelling: KeySignatureSpelling.Value = root match {
//    case Pitch(_, PitchDecorator.Sharp, _) => Sharps
//    case Pitch(_, PitchDecorator.Flat, _) => Flats
//    case Pitch(PitchClass.G, _, _) => Sharps
//    case Pitch(PitchClass.D, _, _) => Sharps
//    case Pitch(PitchClass.A, _, _) => Sharps
//    case Pitch(PitchClass.E, _, _) => Sharps
//    case Pitch(PitchClass.B, _, _) => Sharps
//    case Pitch(PitchClass.C, _, _) => Flats
//    case Pitch(PitchClass.F, _, _) => Flats
//  }
//
//  def getDegreePitch(d: ScaleDegree.Value): Pitch = d match {
//    case ScaleDegree.I => root
//    case ScaleDegree.II => Interval.getPitch(root, IntervalQuality.MajorSecond, getSpelling)
//    case ScaleDegree.III => Interval.getPitch(root, IntervalQuality.MajorThird, getSpelling)
//    case ScaleDegree.IV => Interval.getPitch(root, IntervalQuality.Fourth, getSpelling)
//    case ScaleDegree.V => Interval.getPitch(root, IntervalQuality.Fifth, getSpelling)
//    case ScaleDegree.VI => Interval.getPitch(root, IntervalQuality.MajorSixth, getSpelling)
//    case ScaleDegree.VII => Interval.getPitch(root, IntervalQuality.MinorSeventh, getSpelling)
//  }
//
//  override def asLy: String = s"""\\key ${getDegreePitch(ScaleDegree.IV).asLy} \\major """
//  override def asDSL: String = s"""key ${getDegreePitch(ScaleDegree.IV).asDSL} """
//}
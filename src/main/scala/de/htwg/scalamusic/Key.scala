package de.htwg.scalamusic

import scala.language.postfixOps

object ScaleDegree extends Enumeration {
  type ScaleDegree = Value
  val I, II, III, IIIb, IV, V, Vb, VI, VIb, VII, VIIb, VIII = Value
}

object KeySignatureSpelling extends Enumeration {
  type KeySignatureSpelling = Value
  val Mixed, Sharps, Flats = Value
}

trait Key extends MusicConversion {
  def getSpelling: KeySignatureSpelling.Value
  def getDegreePitch(d: ScaleDegree.Value): Pitch
// getPentatonicScale
  
//  val scale: Seq[Pitch] = ScaleDegree.values.toSeq.map { x => getDegreePitch(x) }

//  def getDegreeIndex(p: Pitch): Int = { val idx = scale.indexOf(p); if (idx != -1) idx else if (getSpelling == KeySignatureSpelling.Flats) scale.indexOf(p.chromaticUp(getSpelling)) else scale.indexOf(p.chromaticDown(getSpelling)) }
//  def stepUp(from: Pitch): Pitch = { val idx = scale.indexOf(from); if (idx < 6) scale(idx + 1) else scale(idx - 6)+ }
//  def stepDown(from: Pitch): Pitch = { val idx = scale.indexOf(from); if (idx > 0) scale(idx - 1) else scale(idx + 6)- }
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
    case ScaleDegree.II => Pitch(root.toPitchNumber + 2, getSpelling)
    case ScaleDegree.III => Pitch(root.toPitchNumber + 4, getSpelling)
    case ScaleDegree.IIIb => Pitch(root.toPitchNumber + 3, Flats)
    case ScaleDegree.IV => Pitch(root.toPitchNumber + 5, getSpelling)
    case ScaleDegree.V => Pitch(root.toPitchNumber + 7, getSpelling)
    case ScaleDegree.Vb => Pitch(root.toPitchNumber + 6, Flats)
    case ScaleDegree.VI => Pitch(root.toPitchNumber + 9, getSpelling)
    case ScaleDegree.VIb => Pitch(root.toPitchNumber + 8, Flats)
    case ScaleDegree.VII => Pitch(root.toPitchNumber + 11, getSpelling)
    case ScaleDegree.VIIb => Pitch(root.toPitchNumber + 10, Flats)
    case ScaleDegree.VIII => Pitch(root.toPitchNumber + 12, getSpelling)
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
    case ScaleDegree.II => Pitch(root.toPitchNumber + 2, getSpelling)
    case ScaleDegree.III => Pitch(root.toPitchNumber + 3, getSpelling)
    case ScaleDegree.IIIb => Pitch(root.toPitchNumber + 3, Flats)
    case ScaleDegree.IV => Pitch(root.toPitchNumber + 5, getSpelling)
    case ScaleDegree.V => Pitch(root.toPitchNumber + 7, getSpelling)
    case ScaleDegree.Vb => Pitch(root.toPitchNumber + 6, Flats)
    case ScaleDegree.VI => Pitch(root.toPitchNumber + 8, getSpelling)
    case ScaleDegree.VIb => Pitch(root.toPitchNumber + 8, Flats)
    case ScaleDegree.VII => Pitch(root.toPitchNumber + 10, getSpelling)
    case ScaleDegree.VIIb => Pitch(root.toPitchNumber + 10, Flats)
    case ScaleDegree.VIII => Pitch(root.toPitchNumber + 12, getSpelling)
  }

  override def asLy: String = s"""\\key ${root.asLy} \\minor """
  override def asDSL: String = s"""key ${root.asDSL}m """
}

//case class MinorPentatonicScale(root: Pitch) extends Key {
//  import KeySignatureSpelling._
//
//  def getDegreePitch(d: ScaleDegree.Value): Pitch = d match {
//    case ScaleDegree.I => root
//    case ScaleDegree.II => Interval.getPitch(root, IntervalQuality.MinorThird, getSpelling)
//    case ScaleDegree.III => Interval.getPitch(root, IntervalQuality.Fourth, getSpelling)
//    case ScaleDegree.IV => Interval.getPitch(root, IntervalQuality.Fifth, getSpelling)
//    case ScaleDegree.V => Interval.getPitch(root, IntervalQuality.MinorSeventh, getSpelling)
//    case ScaleDegree.VI => Interval.getPitch(root, IntervalQuality.Octave, getSpelling)
//    case _ => root
//  }
//
//  def getSpelling: KeySignatureSpelling.Value = root match {
//    case Pitch(_, PitchDecorator.Sharp, _) => Sharps
//    case Pitch(_, PitchDecorator.Flat, _) => Flats
//    case Pitch(PitchClass.C, _, _) => Flats
//    case Pitch(PitchClass.G, _, _) => Sharps
//    case Pitch(PitchClass.D, _, _) => Sharps
//    case Pitch(PitchClass.A, _, _) => Sharps
//    case Pitch(PitchClass.E, _, _) => Sharps
//    case Pitch(PitchClass.B, _, _) => Sharps
//    case Pitch(PitchClass.F, _, _) => Flats
//  }
//
//  override def asLy: String = s"""\\key ${root.asLy} \\major """
//  override def asDSL: String = s"""key ${root.asDSL} """
//}

package de.htwg.scalamusic

import scala.language.postfixOps

object ScaleDegree extends Enumeration {
  type ScaleDegree = Value
  val I, Ib, II, IIb, III, IIIb, IV, IVb, V, Vb, VI, VIb, VII, VIIb, VIII, VIIIb = Value
}

object KeySignatureSpelling extends Enumeration {
  type KeySignatureSpelling = Value
  val Mixed, Sharps, Flats, None = Value
}

trait Key extends MusicConversion {
  def getSpelling: (KeySignatureSpelling.Value, Int)
  def getDegreePitch(d: ScaleDegree.Value): Pitch
}

case class MajorScale(root: Pitch) extends Key {
  import KeySignatureSpelling._
  import ScaleDegree._
  import PitchClass._
  import PitchDecorator._

  def getSpelling: (KeySignatureSpelling.Value, Int) = root match {
    case Pitch(C, Blank, _) => (None, 0)
    case Pitch(G, Blank, _) => (Sharps, 1)
    case Pitch(D, Blank, _) => (Sharps, 2)
    case Pitch(A, Blank, _) => (Sharps, 3)
    case Pitch(E, Blank, _) => (Sharps, 4)
    case Pitch(B, Blank, _) => (Sharps, 5)
    case Pitch(F, Sharp, _) => (Sharps, 6)
    case Pitch(_, Sharp, _) => (Sharps, 7)
    case Pitch(F, Blank, _) => (Flats, 1)
    case Pitch(B, Flat, _) => (Flats, 2)
    case Pitch(E, Flat, _) => (Flats, 3)
    case Pitch(A, Flat, _) => (Flats, 4)
    case Pitch(D, Flat, _) => (Flats, 5)
    case Pitch(G, Flat, _) => (Flats, 6)
    case Pitch(_, Flat, _) => (Flats, 7)
  }

  def getDegreePitch(d: ScaleDegree.Value): Pitch = d match {
    case I => root
    case Ib => Pitch(root.toPitchNumber - 1, getSpelling) //
    case II => Pitch(root.toPitchNumber + 2, getSpelling)
    case IIb => Pitch(root.toPitchNumber + 1, getSpelling) //
    case III => Pitch(root.toPitchNumber + 4, getSpelling)
    case IIIb => Pitch(root.toPitchNumber + 3, getSpelling) //
    case IV => Pitch(root.toPitchNumber + 5, getSpelling)
    case IVb => Pitch(root.toPitchNumber + 4, getSpelling) //
    case V => Pitch(root.toPitchNumber + 7, getSpelling)
    case Vb => Pitch(root.toPitchNumber + 6, getSpelling) //
    case VI => Pitch(root.toPitchNumber + 9, getSpelling)
    case VIb => Pitch(root.toPitchNumber + 8, getSpelling) //
    case VII => Pitch(root.toPitchNumber + 11, getSpelling)
    case VIIb => Pitch(root.toPitchNumber + 10, getSpelling) //
    case VIII => Pitch(root.toPitchNumber + 12, getSpelling)
    case VIIIb => Pitch(root.toPitchNumber + 11, getSpelling) //
  }

  override def asLy: String = s"""\\key ${root.asLy} \\major """
  override def asDSL: String = s"""key ${root.asDSL} """
}

case class MinorScale(root: Pitch) extends Key {
  import KeySignatureSpelling._
  import ScaleDegree._
  import PitchClass._
  import PitchDecorator._

  def getSpelling: (KeySignatureSpelling.Value, Int) = root match {
    case Pitch(A, Blank, _) => (None, 0)
    case Pitch(E, Blank, _) => (Sharps, 1)
    case Pitch(B, Blank, _) => (Sharps, 2)
    case Pitch(F, Sharp, _) => (Sharps, 3)
    case Pitch(C, Sharp, _) => (Sharps, 4)
    case Pitch(G, Sharp, _) => (Sharps, 5)
    case Pitch(D, Sharp, _) => (Sharps, 6)
    case Pitch(_, Sharp, _) => (Sharps, 7)
    case Pitch(D, Blank, _) => (Flats, 1)
    case Pitch(G, Blank, _) => (Flats, 2)
    case Pitch(C, Blank, _) => (Flats, 3)
    case Pitch(F, Blank, _) => (Flats, 4)
    case Pitch(B, Flat, _) => (Flats, 5)
    case Pitch(E, Flat, _) => (Flats, 6)
    case Pitch(_, Flat, _) => (Flats, 7)
  }

  def getDegreePitch(d: ScaleDegree.Value): Pitch = d match {
    case I => root
    case Ib => Pitch(root.toPitchNumber - 1, getSpelling) //
    case II => Pitch(root.toPitchNumber + 2, getSpelling)
    case IIb => Pitch(root.toPitchNumber + 1, getSpelling) //
    case III => Pitch(root.toPitchNumber + 3, getSpelling)
    case IIIb => Pitch(root.toPitchNumber + 3, getSpelling) //
    case IV => Pitch(root.toPitchNumber + 5, getSpelling)
    case IVb => Pitch(root.toPitchNumber + 4, getSpelling) //
    case V => Pitch(root.toPitchNumber + 7, getSpelling)
    case Vb => Pitch(root.toPitchNumber + 6, getSpelling) //
    case VI => Pitch(root.toPitchNumber + 8, getSpelling)
    case VIb => Pitch(root.toPitchNumber + 8, getSpelling) //
    case VII => Pitch(root.toPitchNumber + 10, getSpelling)
    case VIIb => Pitch(root.toPitchNumber + 10, getSpelling) //
    case VIII => Pitch(root.toPitchNumber + 12, getSpelling)
    case VIIIb => Pitch(root.toPitchNumber + 11, getSpelling) //
  }

  override def asLy: String = s"""\\key ${root.asLy} \\minor """
  override def asDSL: String = s"""key ${root.asDSL}m """
}

case class MixolydianScale(root: Pitch) extends Key {
  import KeySignatureSpelling._
  import ScaleDegree._
  import PitchClass._
  import PitchDecorator._

  private val p = MajorScale(getDegreePitch(IV))

  def getSpelling: (KeySignatureSpelling.Value, Int) = root match {
    case Pitch(G, Blank, _) => (None, 0)
    case Pitch(D, Blank, _) => (Sharps, 1)
    case Pitch(A, Blank, _) => (Sharps, 2)
    case Pitch(E, Blank, _) => (Sharps, 3)
    case Pitch(B, Blank, _) => (Sharps, 4)
    case Pitch(F, Sharp, _) => (Sharps, 5)
    case Pitch(C, Sharp, _) => (Sharps, 6)
    case Pitch(_, Sharp, _) => (Sharps, 7)
    case Pitch(C, Blank, _) => (Flats, 1)
    case Pitch(F, Blank, _) => (Flats, 2)
    case Pitch(B, Flat, _) => (Flats, 3)
    case Pitch(E, Flat, _) => (Flats, 4)
    case Pitch(A, Flat, _) => (Flats, 5)
    case Pitch(D, Flat, _) => (Flats, 6)
    case Pitch(_, Flat, _) => (Flats, 7)
  }

  def getDegreePitch(d: ScaleDegree.Value): Pitch = d match {
    case I => root
    case Ib => Pitch(root.toPitchNumber - 1, getSpelling) //
    case II => Pitch(root.toPitchNumber + 2, getSpelling)
    case IIb => Pitch(root.toPitchNumber + 1, getSpelling) //
    case III => Pitch(root.toPitchNumber + 4, getSpelling)
    case IIIb => Pitch(root.toPitchNumber + 3, getSpelling) //
    case IV => Pitch(root.toPitchNumber + 5, getSpelling)
    case IVb => Pitch(root.toPitchNumber + 4, getSpelling) //
    case V => Pitch(root.toPitchNumber + 7, getSpelling)
    case Vb => Pitch(root.toPitchNumber + 6, getSpelling) //
    case VI => Pitch(root.toPitchNumber + 9, getSpelling)
    case VIb => Pitch(root.toPitchNumber + 8, getSpelling) //
    case VII => Pitch(root.toPitchNumber + 10, getSpelling)
    case VIIb => Pitch(root.toPitchNumber + 10, getSpelling) //
    case VIII => Pitch(root.toPitchNumber + 12, getSpelling)
    case VIIIb => Pitch(root.toPitchNumber + 11, getSpelling) //
  }

  override def asLy: String = p.asLy
  override def asDSL: String = p.asDSL
}

case class LocrianScale(root: Pitch) extends Key {
  import KeySignatureSpelling._
  import ScaleDegree._
  import PitchClass._
  import PitchDecorator._

  private val p = MajorScale(getDegreePitch(II))

  def getSpelling: (KeySignatureSpelling.Value, Int) = root match {
    case Pitch(B, Blank, _) => (None, 0)
    case Pitch(F, Sharp, _) => (Sharps, 1)
    case Pitch(C, Sharp, _) => (Sharps, 2)
    case Pitch(G, Sharp, _) => (Sharps, 3)
    case Pitch(D, Sharp, _) => (Sharps, 4)
    case Pitch(A, Sharp, _) => (Sharps, 5)
    case Pitch(E, Sharp, _) => (Sharps, 6)
    case Pitch(_, Sharp, _) => (Sharps, 7)
    case Pitch(E, Blank, _) => (Flats, 1)
    case Pitch(A, Blank, _) => (Flats, 2)
    case Pitch(D, Blank, _) => (Flats, 3)
    case Pitch(G, Blank, _) => (Flats, 4)
    case Pitch(C, Blank, _) => (Flats, 5)
    case Pitch(F, Blank, _) => (Flats, 6)
    case Pitch(_, Flat, _) => (Flats, 7)
  }

  def getDegreePitch(d: ScaleDegree.Value): Pitch = d match {
    case I => root
    case Ib => Pitch(root.toPitchNumber - 1, getSpelling) //
    case II => Pitch(root.toPitchNumber + 1, getSpelling)
    case IIb => Pitch(root.toPitchNumber + 1, getSpelling) //
    case III => Pitch(root.toPitchNumber + 3, getSpelling)
    case IIIb => Pitch(root.toPitchNumber + 3, getSpelling) //
    case IV => Pitch(root.toPitchNumber + 5, getSpelling)
    case IVb => Pitch(root.toPitchNumber + 4, getSpelling) //
    case V => Pitch(root.toPitchNumber + 6, getSpelling)
    case Vb => Pitch(root.toPitchNumber + 6, getSpelling) //
    case VI => Pitch(root.toPitchNumber + 8, getSpelling)
    case VIb => Pitch(root.toPitchNumber + 8, getSpelling) //
    case VII => Pitch(root.toPitchNumber + 10, getSpelling)
    case VIIb => Pitch(root.toPitchNumber + 10, getSpelling) //
    case VIII => Pitch(root.toPitchNumber + 12, getSpelling)
    case VIIIb => Pitch(root.toPitchNumber + 11, getSpelling) //
  }

  override def asLy: String = p.asLy
  override def asDSL: String = p.asDSL
}

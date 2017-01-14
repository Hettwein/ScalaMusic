package de.htwg.scalamusic

import de.htwg.scalamusic._
import scala.language.postfixOps
import ScaleDegree._

case class Chord(root: Pitch = Pitch(), quality: ChordQuality.Value = ChordQuality.Major, duration: Duration = Duration(1, 1), volume: Int = 70) extends MusicElement {
  val scale = getScale;
  //umkehrungen??
  val music = quality match {
    case ChordQuality.Major =>
      Seq(root, scale.getDegreePitch(III), scale.getDegreePitch(V))
    case ChordQuality.Minor =>
      Seq(root, scale.getDegreePitch(III), scale.getDegreePitch(V))
    case ChordQuality.Seventh =>
      Seq(root, scale.getDegreePitch(III), scale.getDegreePitch(V), scale.getDegreePitch(VII))
    case ChordQuality.MajorSeventh =>
      Seq(root, scale.getDegreePitch(III), scale.getDegreePitch(V), scale.getDegreePitch(VII))
    case ChordQuality.MinorSeventh =>
      Seq(root, scale.getDegreePitch(III), scale.getDegreePitch(V), scale.getDegreePitch(VII))
    case ChordQuality.SuspendedSecond =>
      Seq(root, scale.getDegreePitch(II), scale.getDegreePitch(V))
    case ChordQuality.SuspendedFourth =>
      Seq(root, scale.getDegreePitch(IV), scale.getDegreePitch(V))
    case ChordQuality.Sixth =>
      Seq(root, scale.getDegreePitch(III), scale.getDegreePitch(V), scale.getDegreePitch(VI))
    case ChordQuality.Fifth =>
      Seq(root, scale.getDegreePitch(V), scale.getDegreePitch(VIII))
    case ChordQuality.Diminished =>
      Seq(root, scale.getDegreePitch(III), scale.getDegreePitch(V))
    case ChordQuality.Augmented =>
      Seq(root, scale.getDegreePitch(III), scale.getDegreePitch(VIb))
  }

  def getScale(): Key = quality match {
    case ChordQuality.Major => MajorScale(root)
    case ChordQuality.Minor => MinorScale(root)
    case ChordQuality.Seventh => MixolydianScale(root)
    case ChordQuality.MajorSeventh => MajorScale(root)
    case ChordQuality.MinorSeventh => MinorScale(root)
    case ChordQuality.SuspendedSecond => MajorScale(root)
    case ChordQuality.SuspendedFourth => MajorScale(root)
    case ChordQuality.Sixth => MajorScale(root)
    case ChordQuality.Fifth => MajorScale(root) //?
    case ChordQuality.Diminished => LocrianScale(root) //?
    case ChordQuality.Augmented => MajorScale(root) //?
  }

  def asLy: String = {
    val p = "< " + music.foldLeft("")((s, m) => s + m.asLy + " ") + ">"
    val t = duration.getTied.foldLeft("")((s, b) => s + "~ " + p + b.asLy) + " "
    p + duration.asLy + t
  }
  def asDSL: String = {
    val p = root.asDSL + ChordQuality.getAbbr(quality)
    val t = duration.getTied.foldLeft("")((s, b) => s + "~ " + p + ":" + b.asDSL) + " "
    p + duration.asDSL + t
  }
  //  override def asDSL: String = s"""< ${music.foldLeft("")((s, m) => s + m.asDSL + " ")}>${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${if (tied) "~" else ""}"""
}

object Chord {
  def apply(m: Seq[Pitch], duration: Duration, volume: Int): Chord = {
    // calculate chord quality
    Chord()
  }
}

object ChordQuality extends Enumeration {
  type ChordQuality = Value
  val Major, Minor, Diminished, Augmented, Seventh, MajorSeventh, MinorSeventh, Sixth, SuspendedSecond, SuspendedFourth, Fifth, Ninth = Value

  private val abbr = Map(
    "" -> Major,
    "M" -> Major,
    "m" -> Minor,
    ".7" -> Seventh,
    "M7" -> MajorSeventh,
    "m7" -> MinorSeventh,
    ".5" -> Fifth,
    ".6" -> Sixth,
    ".9" -> Ninth,
    ".sus2" -> SuspendedSecond,
    ".sus4" -> SuspendedFourth,
    "aug" -> Augmented,
    "dim" -> Diminished
  )

  def getAbbr(q: Value): String = abbr.map(_.swap).get(q).get
  def apply(s: String): ChordQuality = abbr(s)
}
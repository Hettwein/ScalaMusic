package de.htwg.scalamusic

import de.htwg.scalamusic._

case class Chord(root: Pitch = Pitch(), quality: ChordQuality.Value = ChordQuality.Major, duration: Beat = Beat(1, 1), override val tied: Boolean = false, velocity: Int = 70) extends MusicElement {

  val music = quality match {
    case ChordQuality.Major =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.V))
    case ChordQuality.Minor =>
      val scale = new MinorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.V))
    case ChordQuality.Seventh =>
      val scale = new MajorScale(new MajorScale(root.copy(octave = root.octave - 1)).getDegreePitch(ScaleDegree.IV)); println(scale.getDegreePitch(ScaleDegree.VII)); Seq(root, scale.getDegreePitch(ScaleDegree.VII), scale.getDegreePitch(ScaleDegree.II), scale.getDegreePitch(ScaleDegree.IV))
    case ChordQuality.MajorSeventh =>
      val scale = new MinorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.V), scale.getDegreePitch(ScaleDegree.VII))
    case ChordQuality.SuspendedSecond =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.II), scale.getDegreePitch(ScaleDegree.V))
    case ChordQuality.SuspendedFourth =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.IV), scale.getDegreePitch(ScaleDegree.V))
    case ChordQuality.Sixth =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.V), scale.getDegreePitch(ScaleDegree.VI))
    case ChordQuality.Fifth =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.V), scale.getDegreePitch(ScaleDegree.I).copy(octave = root.octave + 1))
  }

  override def asLy: String = s"""< ${music.foldLeft("")((s, m) => s + m.asLy + " ")}>${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${if (tied) "~" else ""}"""
  override def asDSL: String = s"""${root.asDSL}${ChordQuality.getAbbr(quality)}:${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${if (tied) "~" else ""}"""
  //  override def asDSL: String = s"""< ${music.foldLeft("")((s, m) => s + m.asDSL + " ")}>${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${if (tied) "~" else ""}"""
}

object Chord {
  def apply(m: Seq[Pitch], duration: Beat, velocity: Int): Chord = {
    // calculate chord quality
    Chord()
  }
}

object ChordQuality extends Enumeration {
  type ChordQuality = Value
  val Major, Minor, Diminshed, Augmented, Seventh, MajorSeventh, Sixth, SuspendedSecond, SuspendedFourth, Fifth, Ninth = Value

  private val abbr = Map(
    "" -> Major,
    "M" -> Major,
    "m" -> Minor,
    ".7" -> Seventh,
    "M7" -> MajorSeventh,
    ".5" -> Fifth,
    ".6" -> Sixth,
    ".9" -> Ninth,
    "sus2" -> SuspendedSecond,
    "sus4" -> SuspendedFourth)

  def getAbbr(q: Value): String = abbr.map(_.swap).get(q).get
  def apply(s: String): ChordQuality = abbr(s)
}
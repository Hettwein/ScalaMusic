package de.htwg.scalamusic

import de.htwg.scalamusic._
import scala.language.postfixOps

case class Chord(root: Pitch = Pitch(), quality: ChordQuality.Value = ChordQuality.Major, duration: Beat = Beat(1, 1), velocity: Int = 70) extends MusicElement {
  //umkehrungen??
  val music = quality match {
    case ChordQuality.Major =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.V))
    case ChordQuality.Minor =>
      val scale = new MinorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.V))
    case ChordQuality.Seventh =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.V), scale.getDegreePitch(ScaleDegree.VIIb))
    case ChordQuality.MajorSeventh =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.V), scale.getDegreePitch(ScaleDegree.VII))
    case ChordQuality.MinorSeventh =>
      val scale = new MinorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.V), scale.getDegreePitch(ScaleDegree.VII))
    case ChordQuality.SuspendedSecond =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.II), scale.getDegreePitch(ScaleDegree.V))
    case ChordQuality.SuspendedFourth =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.IV), scale.getDegreePitch(ScaleDegree.V))
    case ChordQuality.Sixth =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.V), scale.getDegreePitch(ScaleDegree.VI))
    case ChordQuality.Fifth =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.V), scale.getDegreePitch(ScaleDegree.VIII))
    case ChordQuality.Diminshed =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.IIIb), scale.getDegreePitch(ScaleDegree.Vb))
    case ChordQuality.Augmented =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.VIb))
  }

  def getScale(): Key = quality match {
    case ChordQuality.Major => MajorScale(root)
    case ChordQuality.Minor => MinorScale(root)
    case ChordQuality.Seventh => MajorScale(root)
    case ChordQuality.MajorSeventh => MajorScale(root)
    case ChordQuality.MinorSeventh => MinorScale(root)
    case ChordQuality.SuspendedSecond => MajorScale(root)
    case ChordQuality.SuspendedFourth => MajorScale(root)
    case ChordQuality.Sixth => MajorScale(root)
    case ChordQuality.Fifth => MajorScale(root)//?
    case ChordQuality.Diminshed => MajorScale(root)//?
    case ChordQuality.Augmented => MajorScale(root)//?
  }

  def asLy: String = {
    val p = "< " + music.foldLeft("")((s, m) => s + m.asLy + " ") + ">"
    val t = duration.getTied.foldLeft("")((s, b) => s + "~ " + p + (if (b.numerator == 1) b.denominator else b.denominator / 2 + ".")) + " "
    s"""${p}${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${t}"""
  }
  def asDSL: String = {
    val p = root.asDSL + ChordQuality.getAbbr(quality)
    val t = duration.getTied.foldLeft("")((s, b) => s + "~ " + p + (if (b.numerator == 1) b.denominator else b.denominator / 2 + ".")) + " "
    s"""${p}${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${t}"""
  }
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
  val Major, Minor, Diminshed, Augmented, Seventh, MajorSeventh, MinorSeventh, Sixth, SuspendedSecond, SuspendedFourth, Fifth, Ninth = Value

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
    ".sus4" -> SuspendedFourth
  )

  def getAbbr(q: Value): String = abbr.map(_.swap).get(q).get
  def apply(s: String): ChordQuality = abbr(s)
}
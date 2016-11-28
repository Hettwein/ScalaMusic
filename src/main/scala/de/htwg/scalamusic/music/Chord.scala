package de.htwg.scalamusic.music

case class Chord(root: Pitch = Pitch(), quality: ChordQuality.Value = ChordQuality.Major, duration: Beat = Beat(1, 1), tied: Boolean = false, velocity: Int = 70) extends MusicElement {

  val music = quality match {
    case ChordQuality.Major =>
      val scale = new MajorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.V))
    case ChordQuality.Minor => val scale = new MinorScale(root); Seq(root, scale.getDegreePitch(ScaleDegree.III), scale.getDegreePitch(ScaleDegree.V))
  }

  override def asLy: String = s"""< ${music.foldLeft("")((s, m) => s + m.asLy + " ")}>${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${if (tied) "~" else ""}"""
  override def asDSL: String = s"""< ${music.foldLeft("")((s, m) => s + m.asDSL + " ")}>${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${if (tied) "~" else ""}"""
}

object Chord {
  def apply(m: Seq[Pitch], duration: Beat, velocity: Int): Chord = {
    // calculate chord quality
    Chord()
  }
}

object ChordQuality extends Enumeration {
  type ChordQuality = Value
  val Major, Minor, Diminshed, Augmented, Seventh, MajorSeventh = Value

  private val abbr = Map(
    "M" -> Major,
    "m" -> Minor
  )

  def apply(s: String): ChordQuality = abbr(s)
}
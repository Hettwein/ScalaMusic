package de.htwg.scalamusic

case class Note(pitch: Pitch = Pitch(), duration: Beat = Beat(), velocity: Int = 70) extends MusicElement {

  override def asLy(): String = {
    val p = pitch.toString().toLowerCase()
    val t = duration.tied.foldLeft("")((s, b) => s + "~ " + p + (if (b.numerator == 1) b.denominator else b.denominator / 2 + ".")) + " "
    s"""${p}${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${if (t.isEmpty) "" else t}"""
  }
  override def asDSL(): String = {
    val p = pitch.toString().toLowerCase()
    val t = duration.tied.foldLeft("")((s, b) => s + "~ " + p + (if (b.numerator == 1) b.denominator else b.denominator / 2 + ".")) + " "
    s"""${p}${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${if (t.isEmpty) "" else t}"""
  }
}
//override def asLy(): String = s"""${pitch.toString().toLowerCase()}${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${if (tied) "~" else ""}"""
//override def asDSL(): String = s"""${pitch.toString().toLowerCase()}${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${if (tied) "~" else ""}"""
//}
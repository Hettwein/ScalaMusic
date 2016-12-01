package de.htwg.scalamusic

case class Note(pitch: Pitch = Pitch(), duration: Beat = Beat(), override val tied: Boolean = false, velocity: Int = 70) extends MusicElement {

  override def asLy(): String = s"""${if (duration.denominator % 3 == 0) "\\tuplet 3/2 " else ""}${pitch.toString().toLowerCase()}${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${if (tied) "~" else ""}"""
  override def asDSL(): String = s"""${pitch.toString().toLowerCase()}${if (duration.numerator == 1) duration.denominator else duration.denominator / 2 + "."}${if (tied) "~" else ""}"""
}
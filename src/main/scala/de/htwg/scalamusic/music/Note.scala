package de.htwg.scalamusic.music

case class Note(pitch: Pitch = Pitch(), duration: Beat = Beat(), velocity: Int = 70) extends MusicElement {
  //  val attribute = Map (
  //    "dot" -> copy(duration = new Beat(3, 2 * this.duration.denominator)),
  //    "tie" -> copy(duration = new Beat(3, 2 * this.duration.denominator))
  //  )
  override def toString() = if (duration.numerator == 1) pitch.toString() + duration.denominator else pitch.toString() + duration.denominator / 2 + "."
}
package de.htwg.scalamusic

case class Note(pitch: Pitch = Pitch(), duration: Duration = Duration(), velocity: Int = 70) extends MusicElement {

  override def asLy(): String = {
    val p = pitch.toString().toLowerCase()
    val t = duration.getTied.foldLeft("")((s, b) => s + "~ " + p + b.asLy) + " "
    p + duration.asDSL + t
  }
  override def asDSL(): String = {
    val p = pitch.toString().toLowerCase()
    val t = duration.getTied.foldLeft("")((s, b) => s + "~ " + p + b.asDSL) + " "
    p + duration.asDSL + t
  }
}
package de.htwg.scalamusic

case class Rest(duration: Duration) extends MusicElement {

  override def asLy(): String = s"""r${duration.asLy}"""
  override def asDSL(): String = s"""r${duration.asDSL}"""
}
package de.htwg.scalamusic

case class Rest(duration: Beat) extends MusicElement {

  override def asLy(): String = s"""r${duration.denominator}"""
  override def asDSL(): String = s"""r${duration.denominator}"""
}
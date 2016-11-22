package de.htwg.scalamusic.music

case class TimeSignature(numerator: Int = 4, denominator: Int = 4) extends MusicDSL {
  override def asLy: String = s"""\\time ${numerator}/${denominator}"""
  override def asDSL: String = s"""<<  >>"""
}

object TimeSignature {
  def apply() = new TimeSignature()
}
package de.htwg.music.elements

import de.htwg.music.MusicElement

case class Rest(value: Double) extends MusicElement {

  def dot = copy(value = value * 1.5)

  val valueToString = Map(
    1 -> "\u1D13B",
    1.5 -> "\u1D13C\u00B7",
    2 -> "\u1D13C",
    8.0/3.0 -> "\u1D13D\u00B7",
    4 -> "\u1D13D",
    16.0/3.0 -> "\u1D13E\u00B7",
    8 -> "\u1D13E",
    16 -> "\u1D13F")

  def play() = {

  }

  override def toString = valueToString(value)
  override def equals(that: Any): Boolean =
    that match {
      case that: Rest => (this.value == that.value)
      case _ => false
    }
}
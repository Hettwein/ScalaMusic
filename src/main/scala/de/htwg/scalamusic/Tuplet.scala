package de.htwg.scalamusic

case class Tuplet(number: Int = 3, elements: Seq[MusicElement]) extends MusicElement {
  
  val duration: Beat =  elements.foldLeft(Beat(0, 1))((n, m) => n.sum(m.duration)).mul(Beat(2, number))
  override def asLy(): String = s"""\\tuplet ${number}/2 { ${elements.foldLeft("")((s, m) => s + m.asLy + " ")}} """
  override def asDSL(): String = s"""( ${elements.foldLeft("")((s, m) => s + m.asDSL + " ")})${number}"""

}
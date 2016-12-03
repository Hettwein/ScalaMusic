package de.htwg.scalamusic

case class Pattern(elements: Seq[MusicElement]) extends MusicElement {

  val duration: Beat = elements.foldLeft(Beat(0, 1))((n, m) => n.sum(m.duration))

  override def asLy(): String = s"""${elements.foldLeft("")((s, m) => s + m.asLy + " ")}"""
  override def asDSL(): String = s"""${elements.foldLeft("")((s, m) => s + m.asDSL + " ")}"""

}
package de.htwg.music

case class Sign(pos: Vector[Int], mode: SignMode.Value) {
  
}

object SignMode extends Enumeration {
  type SignMode = Value
  val None, Natural, Sharp, Flat, DoubleSharp, DoubleFlat = Value
}
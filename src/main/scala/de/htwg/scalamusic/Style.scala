package de.htwg.scalamusic

trait Style extends MusicConversion {

  val name: String
  val pattern: Map[Int, Seq[(ScaleDegree.Value, Beat)]]
  val fill: Map[Int, Seq[(ScaleDegree.Value, Beat)]]

  def asLy: String = ""
  def asDSL: String = s"""style '${name}' """
}

object Rock extends Style {
  val name: String = "rock"
  val pattern = Map(
    0 -> Seq((ScaleDegree.I, Beat(1, 8))),
    1 -> Seq((ScaleDegree.I, Beat(3, 8)), (ScaleDegree.I, Beat(1, 8))))

  val fill = Map(
    0 -> Seq((ScaleDegree.I, Beat(3, 8)), (ScaleDegree.I, Beat(1, 8)), (ScaleDegree.I, Beat(1, 2))))
}

object Funk extends Style {
  val name: String = "funk"
  val pattern = Map(
    0 -> Seq((ScaleDegree.I, Beat(1, 4)), (ScaleDegree.VIII, Beat(1, 4))))

  val fill = Map(
    0 -> Seq((ScaleDegree.I, Beat(1, 4)), (ScaleDegree.VIII, Beat(1, 4))))
}

object Jazz extends Style {
  val name: String = "jazz"
  val pattern = Map(
    0 -> Seq((ScaleDegree.I, Beat(1, 4))))

  val fill = Map(
    0 -> Seq((ScaleDegree.I, Beat(1, 4))))
}
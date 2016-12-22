package de.htwg.scalamusic

trait Style extends MusicConversion {
  val name: String
  val pattern: Map[Int, Seq[(ScaleDegree.Value, Beat)]]
  val fill: Map[Int, Seq[(ScaleDegree.Value, Beat)]]

  def asLy: String = ""
  def asDSL: String = s"""style '${name}' """
}

object Rock extends Style {
  import ScaleDegree._
  val name: String = "rock"
  val pattern = Map(
    0 -> Seq((I, Beat(1, 8))),
    1 -> Seq((I, Beat(3, 8)), (I, Beat(1, 8))))

  val fill = Map(
    0 -> Seq((I, Beat(3, 8)), (I, Beat(1, 8)), (I, Beat(1, 2))))
}

object Funk extends Style {
  import ScaleDegree._
  val name: String = "funk"
  val pattern = Map(
    0 -> Seq((I, Beat(1, 4)), (VIII, Beat(1, 4))),
    1 -> Seq((I, Beat(1, 4)), (null, Beat(1, 8)), (null, Beat(1, 16)), (I, Beat(1, 16)), (I, Beat(1, 8)), (I, Beat(1, 8)), (null, Beat(1, 4))),
    2 -> Seq((I, Beat(1, 16)), (I, Beat(1, 16)), (null, Beat(1, 8)), (null, Beat(1, 8)), (null, Beat(1, 16)), (I, Beat(1, 16)), (null, Beat(1, 2))),
    3 -> Seq((I, Beat(1, 8)), (VIII, Beat(1, 8)), (null, Beat(1, 8)), (I, Beat(1, 16)), (I, Beat(1, 16)), (null, Beat(1, 2))))

  val fill = Map(
    0 -> Seq((I, Beat(1, 8)), (I, Beat(1, 8)), (VIII, Beat(1, 16)), (VIIb, Beat(1, 16)), (V, Beat(1, 16)), (I, Beat(1, 16)), (null, Beat(1, 8)), (null, Beat(1, 16)), (IIIb, Beat(1, 16)), (null, Beat(1, 4))))
}

object Jazz extends Style {
  import ScaleDegree._
  val name: String = "jazz"
  val pattern = Map(
    0 -> Seq((I, Beat(1, 4))),
    1 -> Seq((I, Beat(1, 4)), (III, Beat(1, 4)), (V, Beat(1, 4)), (VI, Beat(1, 4))),
    2 -> Seq((VIII, Beat(1, 4)), (VI, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4))),
    3 -> Seq((I, Beat(1, 4)), (III, Beat(1, 4)), (V, Beat(1, 4)), (VI, Beat(1, 4)), (VIII, Beat(1, 4)), (VI, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4))),
    4 -> Seq((VIII, Beat(1, 4)), (VII, Beat(1, 4)), (VI, Beat(1, 4)), (V, Beat(1, 4))))

  val fill = Map(
    0 -> Seq((VIII, Beat(1, 4)), (VI, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4))))
}

object SlowRock extends Style {
  import ScaleDegree._
  val name: String = "slowRock"
  val pattern = Map(
    0 -> Seq((I, Beat(1, 4)), (I, Beat(1, 6)), (III, Beat(1, 12)), (V, Beat(1, 4)), (III, Beat(1, 6)), (V, Beat(1, 12))))

  val fill = Map(
    0 -> Seq((I, Beat(1, 4)), (I, Beat(1, 6)), (III, Beat(1, 12)), (V, Beat(1, 4)), (III, Beat(1, 6)), (V, Beat(1, 12))))
}
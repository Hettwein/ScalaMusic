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

object Funk extends Style { //http://www.bassplayer.com/styles/1181/what-the-funk/25901
  import ScaleDegree._
  val name: String = "funk"
  val pattern = Map(
    0 -> Seq((I, Beat(1, 4)), (VIII, Beat(1, 4))),
    1 -> Seq((I, Beat(1, 4)), (null, Beat(1, 8)), (null, Beat(1, 16)), (I, Beat(1, 16)), (I, Beat(1, 8)), (I, Beat(1, 8)), (null, Beat(1, 4))),
    2 -> Seq((I, Beat(1, 16)), (I, Beat(1, 16)), (null, Beat(1, 8)), (null, Beat(1, 8)), (null, Beat(1, 16)), (I, Beat(1, 16)), (null, Beat(1, 2))),
    3 -> Seq((I, Beat(1, 8)), (VIII, Beat(1, 8)), (null, Beat(1, 8)), (I, Beat(1, 16)), (I, Beat(1, 16)), (null, Beat(1, 2))),
    4 -> Seq((I, Beat(1, 4, Beat(1, 8, Beat(1, 16)))), (I, Beat(1, 16)), (I, Beat(1, 8)), (I, Beat(1, 8, Beat(1, 4)))))

  val fill = Map(
    0 -> Seq((I, Beat(1, 8)), (I, Beat(1, 8)), (VIII, Beat(1, 16)), (VIIb, Beat(1, 16)), (V, Beat(1, 16)), (I, Beat(1, 16)), (null, Beat(1, 8)), (null, Beat(1, 16)), (IIIb, Beat(1, 16)), (null, Beat(1, 4))),
    1 -> Seq((I, Beat(1, 4, Beat(1, 8, Beat(1, 16)))), (V, Beat(1, 16)), (VIIb, Beat(1, 16)), (V, Beat(1, 16)), (VI, Beat(1, 16)), (V, Beat(1, 16)), (IV, Beat(1, 16)), (V, Beat(1, 16)), (IIIb, Beat(1, 16)), (III, Beat(1, 16))))
}

object Jazz extends Style {
  import ScaleDegree._
  val name: String = "jazz"
  val pattern = Map(
    0 -> Seq((I, Beat(1, 4))),
    1 -> Seq((I, Beat(1, 4)), (I, Beat(1, 4)), (V, Beat(1, 4)), (V, Beat(1, 4))),
    2 -> Seq((I, Beat(1, 4)), (II, Beat(1, 4)), (III, Beat(1, 4)), (I, Beat(1, 4))),
    3 -> Seq((I, Beat(1, 4)), (III, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4))),
    4 -> Seq((I, Beat(1, 4)), (II, Beat(1, 4)), (I, Beat(1, 4)), (V, Beat(1, 4))),
    5 -> Seq((I, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4)), (I, Beat(1, 4))),
    6 -> Seq((VIII, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4)), (I, Beat(1, 4))),
    7 -> Seq((VIII, Beat(1, 4)), (VIII, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4))),
    8 -> Seq((I, Beat(1, 4)), (II, Beat(1, 4)), (III, Beat(1, 4)), (V, Beat(1, 4))),
    9 -> Seq((VIII, Beat(1, 4)), (VII, Beat(1, 4)), (VI, Beat(1, 4)), (V, Beat(1, 4))),
    10 -> Seq((I, Beat(1, 4)), (V, Beat(1, 4)), (VIII, Beat(1, 4)), (V, Beat(1, 4))),
    11 -> Seq((IV, Beat(1, 4)), (III, Beat(1, 4)), (II, Beat(1, 4)), (I, Beat(1, 4))),
    12 -> Seq((I, Beat(1, 4)), (I, Beat(1, 4)), (II, Beat(1, 4)), (I, Beat(1, 4))),
    13 -> Seq((I, Beat(1, 4)), (III, Beat(1, 4)), (V, Beat(1, 4)), (VIII, Beat(1, 4))),
    14 -> Seq((I, Beat(1, 4)), (V, Beat(1, 4)), (VII, Beat(1, 4)), (VIII, Beat(1, 4))),
    15 -> Seq((VIII, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4)), (V, Beat(1, 4))),
    16 -> Seq((I, Beat(1, 4)), (I, Beat(1, 4)), (V, Beat(1, 4)), (I, Beat(1, 4))),
    17 -> Seq((VIII, Beat(1, 4)), (V, Beat(1, 4)), (I, Beat(1, 4)), (III, Beat(1, 4))))

  val fill = Map(
    0 -> Seq((I, Beat(1, 4))),
    1 -> Seq((I, Beat(1, 4)), (I, Beat(1, 4)), (V, Beat(1, 4)), (V, Beat(1, 4))),
    2 -> Seq((I, Beat(1, 4)), (II, Beat(1, 4)), (III, Beat(1, 4)), (I, Beat(1, 4))),
    3 -> Seq((I, Beat(1, 4)), (III, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4))),
    4 -> Seq((I, Beat(1, 4)), (II, Beat(1, 4)), (I, Beat(1, 4)), (V, Beat(1, 4))),
    5 -> Seq((I, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4)), (I, Beat(1, 4))),
    6 -> Seq((VIII, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4)), (I, Beat(1, 4))),
    7 -> Seq((VIII, Beat(1, 4)), (VIII, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4))),
    8 -> Seq((I, Beat(1, 4)), (II, Beat(1, 4)), (III, Beat(1, 4)), (V, Beat(1, 4))),
    9 -> Seq((VIII, Beat(1, 4)), (VII, Beat(1, 4)), (VI, Beat(1, 4)), (V, Beat(1, 4))),
    10 -> Seq((I, Beat(1, 4)), (V, Beat(1, 4)), (VIII, Beat(1, 4)), (V, Beat(1, 4))),
    11 -> Seq((IV, Beat(1, 4)), (III, Beat(1, 4)), (II, Beat(1, 4)), (I, Beat(1, 4))),
    12 -> Seq((I, Beat(1, 4)), (I, Beat(1, 4)), (II, Beat(1, 4)), (I, Beat(1, 4))),
    13 -> Seq((I, Beat(1, 4)), (III, Beat(1, 4)), (V, Beat(1, 4)), (VIII, Beat(1, 4))),
    14 -> Seq((I, Beat(1, 4)), (V, Beat(1, 4)), (VII, Beat(1, 4)), (VIII, Beat(1, 4))),
    15 -> Seq((VIII, Beat(1, 4)), (V, Beat(1, 4)), (III, Beat(1, 4)), (V, Beat(1, 4))),
    16 -> Seq((I, Beat(1, 4)), (I, Beat(1, 4)), (V, Beat(1, 4)), (I, Beat(1, 4))),
    17 -> Seq((VIII, Beat(1, 4)), (V, Beat(1, 4)), (I, Beat(1, 4)), (III, Beat(1, 4))))
}

object SlowRock extends Style {
  import ScaleDegree._
  val name: String = "slowRock"
  val pattern = Map(
    0 -> Seq((I, Beat(1, 4)), (I, Beat(1, 6)), (III, Beat(1, 12)), (V, Beat(1, 4)), (III, Beat(1, 6)), (V, Beat(1, 12))))

  val fill = Map(
    0 -> Seq((I, Beat(1, 4)), (I, Beat(1, 6)), (III, Beat(1, 12)), (V, Beat(1, 4)), (III, Beat(1, 6)), (V, Beat(1, 12))))
}
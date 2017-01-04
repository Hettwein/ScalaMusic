package de.htwg.scalamusic

trait Style extends MusicConversion {
  val name: String
  val pattern: Map[Int, Seq[(ScaleDegree.Value, Duration)]] //octave
  val fill: Map[Int, Seq[(ScaleDegree.Value, Duration)]]

  def asLy: String = ""
  def asDSL: String = s"""style '${name}' """
}

object Rock extends Style {
  import ScaleDegree._
  val name: String = "rock"
  val pattern = Map(
    0 -> Seq((I, Duration(1, 8))),
    1 -> Seq((I, Duration(3, 8)), (I, Duration(1, 8))),
    2 -> Seq((I, Duration(1, 8)), (I, Duration(1, 8, Duration(1, 8))), (I, Duration(1, 8))),
    3 -> Seq((I, Duration(1, 4)), (I, Duration(1, 4)), (I, Duration(1, 4)), (null, Duration(1, 8)), (I, Duration(1, 8, Duration(1, 8))), (I, Duration(1, 8, Duration(1, 8))), (I, Duration(1, 8)), (I, Duration(1, 8)), (I, Duration(1, 8)), (I, Duration(1, 8)), (I, Duration(1, 8))))

  val fill = Map(
    0 -> Seq((I, Duration(3, 8)), (I, Duration(1, 8)), (I, Duration(1, 2))))
}

object Funk extends Style { //http://www.bassplayer.com/styles/1181/what-the-funk/25901
  import ScaleDegree._
  val name: String = "funk"
  val pattern = Map(
    0 -> Seq((I, Duration(1, 4)), (VIII, Duration(1, 4))),
    1 -> Seq((I, Duration(1, 4)), (null, Duration(1, 8)), (null, Duration(1, 16)), (I, Duration(1, 16)), (I, Duration(1, 8)), (I, Duration(1, 8)), (null, Duration(1, 4))),
    2 -> Seq((I, Duration(1, 16)), (I, Duration(1, 16)), (null, Duration(1, 8)), (null, Duration(1, 8)), (null, Duration(1, 16)), (I, Duration(1, 16)), (null, Duration(1, 2))),
    3 -> Seq((I, Duration(1, 8)), (VIII, Duration(1, 8)), (null, Duration(1, 8)), (I, Duration(1, 16)), (I, Duration(1, 16)), (null, Duration(1, 2))),
    4 -> Seq((I, Duration(1, 4, Duration(1, 8, Duration(1, 16)))), (I, Duration(1, 16)), (I, Duration(1, 8)), (I, Duration(1, 8, Duration(1, 4)))))

  val fill = Map(
    0 -> Seq((I, Duration(1, 8)), (I, Duration(1, 8)), (VIII, Duration(1, 16)), (VIIb, Duration(1, 16)), (V, Duration(1, 16)), (I, Duration(1, 16)), (null, Duration(1, 8)), (null, Duration(1, 16)), (IIIb, Duration(1, 16)), (null, Duration(1, 4))),
    1 -> Seq((I, Duration(1, 4, Duration(1, 8, Duration(1, 16)))), (V, Duration(1, 16)), (VIIb, Duration(1, 16)), (V, Duration(1, 16)), (VI, Duration(1, 16)), (V, Duration(1, 16)), (IV, Duration(1, 16)), (V, Duration(1, 16)), (IIIb, Duration(1, 16)), (III, Duration(1, 16))))
}

object Jazz extends Style {
  import ScaleDegree._
  val name: String = "jazz"
  val pattern = Map(
    0 -> Seq((I, Duration(1, 4))),
    1 -> Seq((I, Duration(1, 4)), (I, Duration(1, 4)), (V, Duration(1, 4)), (V, Duration(1, 4))),
    2 -> Seq((I, Duration(1, 4)), (II, Duration(1, 4)), (III, Duration(1, 4)), (I, Duration(1, 4))),
    3 -> Seq((I, Duration(1, 4)), (III, Duration(1, 4)), (V, Duration(1, 4)), (III, Duration(1, 4))),
    4 -> Seq((I, Duration(1, 4)), (II, Duration(1, 4)), (I, Duration(1, 4)), (V, Duration(1, 4))),
    5 -> Seq((I, Duration(1, 4)), (V, Duration(1, 4)), (III, Duration(1, 4)), (I, Duration(1, 4))),
    6 -> Seq((VIII, Duration(1, 4)), (V, Duration(1, 4)), (III, Duration(1, 4)), (I, Duration(1, 4))),
    7 -> Seq((VIII, Duration(1, 4)), (VIII, Duration(1, 4)), (V, Duration(1, 4)), (III, Duration(1, 4))),
    8 -> Seq((I, Duration(1, 4)), (II, Duration(1, 4)), (III, Duration(1, 4)), (V, Duration(1, 4))),
    9 -> Seq((VIII, Duration(1, 4)), (VII, Duration(1, 4)), (VI, Duration(1, 4)), (V, Duration(1, 4))),
    10 -> Seq((I, Duration(1, 4)), (V, Duration(1, 4)), (VIII, Duration(1, 4)), (V, Duration(1, 4))),
    11 -> Seq((IV, Duration(1, 4)), (III, Duration(1, 4)), (II, Duration(1, 4)), (I, Duration(1, 4))),
    12 -> Seq((I, Duration(1, 4)), (I, Duration(1, 4)), (II, Duration(1, 4)), (I, Duration(1, 4))),
    13 -> Seq((I, Duration(1, 4)), (III, Duration(1, 4)), (V, Duration(1, 4)), (VIII, Duration(1, 4))),
    14 -> Seq((I, Duration(1, 4)), (V, Duration(1, 4)), (VII, Duration(1, 4)), (VIII, Duration(1, 4))),
    15 -> Seq((VIII, Duration(1, 4)), (V, Duration(1, 4)), (III, Duration(1, 4)), (V, Duration(1, 4))),
    16 -> Seq((I, Duration(1, 4)), (I, Duration(1, 4)), (V, Duration(1, 4)), (I, Duration(1, 4))),
    17 -> Seq((VIII, Duration(1, 4)), (V, Duration(1, 4)), (I, Duration(1, 4)), (III, Duration(1, 4))))

  val fill = Map(
    0 -> Seq((I, Duration(1, 4))),
    1 -> Seq((I, Duration(1, 4)), (I, Duration(1, 4)), (V, Duration(1, 4)), (V, Duration(1, 4))),
    2 -> Seq((I, Duration(1, 4)), (II, Duration(1, 4)), (III, Duration(1, 4)), (I, Duration(1, 4))),
    3 -> Seq((I, Duration(1, 4)), (III, Duration(1, 4)), (V, Duration(1, 4)), (III, Duration(1, 4))),
    4 -> Seq((I, Duration(1, 4)), (II, Duration(1, 4)), (I, Duration(1, 4)), (V, Duration(1, 4))),
    5 -> Seq((I, Duration(1, 4)), (V, Duration(1, 4)), (III, Duration(1, 4)), (I, Duration(1, 4))),
    6 -> Seq((VIII, Duration(1, 4)), (V, Duration(1, 4)), (III, Duration(1, 4)), (I, Duration(1, 4))),
    7 -> Seq((VIII, Duration(1, 4)), (VIII, Duration(1, 4)), (V, Duration(1, 4)), (III, Duration(1, 4))),
    8 -> Seq((I, Duration(1, 4)), (II, Duration(1, 4)), (III, Duration(1, 4)), (V, Duration(1, 4))),
    9 -> Seq((VIII, Duration(1, 4)), (VII, Duration(1, 4)), (VI, Duration(1, 4)), (V, Duration(1, 4))),
    10 -> Seq((I, Duration(1, 4)), (V, Duration(1, 4)), (VIII, Duration(1, 4)), (V, Duration(1, 4))),
    11 -> Seq((IV, Duration(1, 4)), (III, Duration(1, 4)), (II, Duration(1, 4)), (I, Duration(1, 4))),
    12 -> Seq((I, Duration(1, 4)), (I, Duration(1, 4)), (II, Duration(1, 4)), (I, Duration(1, 4))),
    13 -> Seq((I, Duration(1, 4)), (III, Duration(1, 4)), (V, Duration(1, 4)), (VIII, Duration(1, 4))),
    14 -> Seq((I, Duration(1, 4)), (V, Duration(1, 4)), (VII, Duration(1, 4)), (VIII, Duration(1, 4))),
    15 -> Seq((VIII, Duration(1, 4)), (V, Duration(1, 4)), (III, Duration(1, 4)), (V, Duration(1, 4))),
    16 -> Seq((I, Duration(1, 4)), (I, Duration(1, 4)), (V, Duration(1, 4)), (I, Duration(1, 4))),
    17 -> Seq((VIII, Duration(1, 4)), (V, Duration(1, 4)), (I, Duration(1, 4)), (III, Duration(1, 4))))
}

object Swing extends Style {
  import ScaleDegree._
  val name: String = "swing"
  val pattern = Map(
    0 -> Seq((I, Duration(1, 4)), (null, Duration(1, 6)), (I, Duration(1, 12))))

  val fill = Map(
    0 -> Seq((I, Duration(1, 4)), (null, Duration(1, 6)), (I, Duration(1, 12)), (I, Duration(1, 6)), (I, Duration(1, 12)), (II, Duration(1, 6)), (III, Duration(1, 12))))
}

object SlowRock extends Style {
  import ScaleDegree._
  val name: String = "slowRock"
  val pattern = Map(
    0 -> Seq((I, Duration(1, 4)), (I, Duration(1, 6)), (III, Duration(1, 12)), (V, Duration(1, 4)), (III, Duration(1, 6)), (V, Duration(1, 12))),
    1 -> Seq((I, Duration(3, 4)), (I, Duration(1, 4))))

  val fill = Map(
    0 -> Seq((I, Duration(1, 4)), (I, Duration(1, 6)), (III, Duration(1, 12)), (V, Duration(1, 4)), (III, Duration(1, 6)), (V, Duration(1, 12))),
    1 -> Seq((I, Duration(3, 4)), (I, Duration(1, 4))),
    2 -> Seq((VIII, Duration(1, 4)), (VIIb, Duration(1, 4)), (VI, Duration(1, 4)), (V, Duration(1, 4))))
}

object Boogie extends Style {
  import ScaleDegree._
  val name: String = "boogie"
  val pattern = Map(
    0 -> Seq((I, Duration(1, 8)), (I, Duration(1, 8)), (III, Duration(1, 8)), (III, Duration(1, 8)), (V, Duration(1, 8)), (V, Duration(1, 8)), (VI, Duration(1, 8)), (V, Duration(1, 8))),
    1 -> Seq((I, Duration(1, 4)), (null, Duration(1, 8)), (III, Duration(1, 8)), (null, Duration(1, 4)), (V, Duration(1, 4))))

  val fill = Map(
    0 -> Seq((I, Duration(1, 8)), (I, Duration(1, 8)), (III, Duration(1, 8)), (III, Duration(1, 8)), (V, Duration(1, 8)), (V, Duration(1, 8)), (VI, Duration(1, 8)), (V, Duration(1, 8))),
    1 -> Seq((null, Duration(1, 8)), (I, Duration(1, 8)), (I, Duration(1, 8)), (I, Duration(1, 8)), (I, Duration(1, 8)), (I, Duration(1, 8)), (I, Duration(1, 8)), (I, Duration(1, 8))),
    2 -> Seq((VIII, Duration(1, 4)), (null, Duration(1, 8)), (VI, Duration(1, 8)), (null, Duration(1, 4)), (V, Duration(1, 4))))
}

object Reggae extends Style {
  import ScaleDegree._
  val name: String = "reggae"
  val pattern = Map(
    0 -> Seq((null, Duration(3, 16)), (VIII, Duration(1, 16)), (VI, Duration(1, 8)), (VIII, Duration(1, 16)), (null, Duration(1, 16)), (null, Duration(3, 16)), (VIII, Duration(1, 16)), (VI, Duration(1, 8)), (V, Duration(1, 16)), (null, Duration(1, 16))))

  val fill = Map(
    0 -> Seq((null, Duration(3, 16)), (VIII, Duration(1, 16)), (VI, Duration(1, 8)), (VIII, Duration(1, 16)), (null, Duration(1, 16)), (null, Duration(3, 16)), (VIII, Duration(1, 16)), (VI, Duration(1, 8)), (V, Duration(1, 16)), (null, Duration(1, 16))))
}
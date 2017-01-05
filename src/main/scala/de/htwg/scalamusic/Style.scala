package de.htwg.scalamusic

trait Style extends MusicConversion {
  val name: String
  val pattern: Map[Int, Seq[Part]]
  val fill: Map[Int, Seq[Part]]

  def d(a: Int, b: Int, c: Duration = null) = Duration(a, b, c)
  def p(a: ScaleDegree.Value, b: Duration, c: Int = 0) = Part(b, a, c)

  def asLy: String = ""
  def asDSL: String = s"""style '${name}' """
}

object Rock extends Style {
  import ScaleDegree._
  val name: String = "rock"
  val pattern = Map(
    0 -> Seq(p(I, d(1, 8))),
    1 -> Seq(p(I, d(3, 8)), p(I, d(1, 8))),
    2 -> Seq(p(I, d(1, 8)), p(I, d(1, 8, d(1, 8))), p(I, d(1, 8))),
    3 -> Seq(p(I, d(1, 4)), p(I, d(1, 4)), p(I, d(1, 4)), p(null, d(1, 8)), p(I, d(1, 8, d(1, 8))), p(I, d(1, 8, d(1, 8))), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8))))

  val fill = Map(
    0 -> Seq(p(I, d(3, 8)), p(I, d(1, 8)), p(I, d(1, 2))))
}

object Funk extends Style { //http://www.bassplayer.com/styles/1181/what-the-funk/25901
  import ScaleDegree._
  val name: String = "funk"
  val pattern = Map(
    0 -> Seq(p(I, d(1, 4)), p(VIII, d(1, 4))),
    1 -> Seq(p(I, d(1, 4)), p(null, d(1, 8)), p(null, d(1, 16)), p(I, d(1, 16)), p(I, d(1, 8)), p(I, d(1, 8)), p(null, d(1, 4))),
    2 -> Seq(p(I, d(1, 16)), p(I, d(1, 16)), p(null, d(1, 8)), p(null, d(1, 8)), p(null, d(1, 16)), p(I, d(1, 16)), p(null, d(1, 2))),
    3 -> Seq(p(I, d(1, 8)), p(VIII, d(1, 8)), p(null, d(1, 8)), p(I, d(1, 16)), p(I, d(1, 16)), p(null, d(1, 2))),
    4 -> Seq(p(I, d(1, 4, d(1, 8, d(1, 16)))), p(I, d(1, 16)), p(I, d(1, 8)), p(I, d(1, 8, d(1, 4)))))

  val fill = Map(
    0 -> Seq(p(I, d(1, 8)), p(I, d(1, 8)), p(VIII, d(1, 16)), p(VIIb, d(1, 16)), p(V, d(1, 16)), p(I, d(1, 16)), p(null, d(1, 8)), p(null, d(1, 16)), p(IIIb, d(1, 16)), p(null, d(1, 4))),
    1 -> Seq(p(I, d(1, 4, d(1, 8, d(1, 16)))), p(V, d(1, 16)), p(VIIb, d(1, 16)), p(V, d(1, 16)), p(VI, d(1, 16)), p(V, d(1, 16)), p(IV, d(1, 16)), p(V, d(1, 16)), p(IIIb, d(1, 16)), p(III, d(1, 16))))
}

object Jazz extends Style {
  import ScaleDegree._
  val name: String = "jazz"
  val pattern = Map(
    0 -> Seq(p(I, d(1, 4))),
    1 -> Seq(p(I, d(1, 4)), p(I, d(1, 4)), p(V, d(1, 4)), p(V, d(1, 4))),
    2 -> Seq(p(I, d(1, 4)), p(II, d(1, 4)), p(III, d(1, 4)), p(I, d(1, 4))),
    3 -> Seq(p(I, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4))),
    4 -> Seq(p(I, d(1, 4)), p(II, d(1, 4)), p(I, d(1, 4)), p(V, d(1, 4))),
    5 -> Seq(p(I, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4)), p(I, d(1, 4))),
    6 -> Seq(p(VIII, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4)), p(I, d(1, 4))),
    7 -> Seq(p(VIII, d(1, 4)), p(VIII, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4))),
    8 -> Seq(p(I, d(1, 4)), p(II, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4))),
    9 -> Seq(p(VIII, d(1, 4)), p(VII, d(1, 4)), p(VI, d(1, 4)), p(V, d(1, 4))),
    10 -> Seq(p(I, d(1, 4)), p(V, d(1, 4)), p(VIII, d(1, 4)), p(V, d(1, 4))),
    11 -> Seq(p(IV, d(1, 4)), p(III, d(1, 4)), p(II, d(1, 4)), p(I, d(1, 4))),
    12 -> Seq(p(I, d(1, 4)), p(I, d(1, 4)), p(II, d(1, 4)), p(I, d(1, 4))),
    13 -> Seq(p(I, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4)), p(VIII, d(1, 4))),
    14 -> Seq(p(I, d(1, 4)), p(V, d(1, 4)), p(VII, d(1, 4)), p(VIII, d(1, 4))),
    15 -> Seq(p(VIII, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4))),
    16 -> Seq(p(I, d(1, 4)), p(I, d(1, 4)), p(V, d(1, 4)), p(I, d(1, 4))),
    17 -> Seq(p(VIII, d(1, 4)), p(V, d(1, 4)), p(I, d(1, 4)), p(III, d(1, 4))))

  val fill = Map(
    0 -> Seq(p(I, d(1, 4))),
    1 -> Seq(p(I, d(1, 4)), p(I, d(1, 4)), p(V, d(1, 4)), p(V, d(1, 4))),
    2 -> Seq(p(I, d(1, 4)), p(II, d(1, 4)), p(III, d(1, 4)), p(I, d(1, 4))),
    3 -> Seq(p(I, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4))),
    4 -> Seq(p(I, d(1, 4)), p(II, d(1, 4)), p(I, d(1, 4)), p(V, d(1, 4))),
    5 -> Seq(p(I, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4)), p(I, d(1, 4))),
    6 -> Seq(p(VIII, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4)), p(I, d(1, 4))),
    7 -> Seq(p(VIII, d(1, 4)), p(VIII, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4))),
    8 -> Seq(p(I, d(1, 4)), p(II, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4))),
    9 -> Seq(p(VIII, d(1, 4)), p(VII, d(1, 4)), p(VI, d(1, 4)), p(V, d(1, 4))),
    10 -> Seq(p(I, d(1, 4)), p(V, d(1, 4)), p(VIII, d(1, 4)), p(V, d(1, 4))),
    11 -> Seq(p(IV, d(1, 4)), p(III, d(1, 4)), p(II, d(1, 4)), p(I, d(1, 4))),
    12 -> Seq(p(I, d(1, 4)), p(I, d(1, 4)), p(II, d(1, 4)), p(I, d(1, 4))),
    13 -> Seq(p(I, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4)), p(VIII, d(1, 4))),
    14 -> Seq(p(I, d(1, 4)), p(V, d(1, 4)), p(VII, d(1, 4)), p(VIII, d(1, 4))),
    15 -> Seq(p(VIII, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4))),
    16 -> Seq(p(I, d(1, 4)), p(I, d(1, 4)), p(V, d(1, 4)), p(I, d(1, 4))),
    17 -> Seq(p(VIII, d(1, 4)), p(V, d(1, 4)), p(I, d(1, 4)), p(III, d(1, 4))))
}

object Swing extends Style {
  import ScaleDegree._
  val name: String = "swing"
  val pattern = Map(
    0 -> Seq(p(I, d(1, 4)), p(null, d(1, 6)), p(I, d(1, 12))))

  val fill = Map(
    0 -> Seq(p(I, d(1, 4)), p(null, d(1, 6)), p(I, d(1, 12)), p(I, d(1, 6)), p(I, d(1, 12)), p(II, d(1, 6)), p(III, d(1, 12))))
}

object SlowRock extends Style {
  import ScaleDegree._
  val name: String = "slowRock"
  val pattern = Map(
    0 -> Seq(p(I, d(1, 4)), p(I, d(1, 6)), p(III, d(1, 12)), p(V, d(1, 4)), p(III, d(1, 6)), p(V, d(1, 12))),
    1 -> Seq(p(I, d(3, 4)), p(I, d(1, 4))))

  val fill = Map(
    0 -> Seq(p(I, d(1, 4)), p(I, d(1, 6)), p(III, d(1, 12)), p(V, d(1, 4)), p(III, d(1, 6)), p(V, d(1, 12))),
    1 -> Seq(p(I, d(3, 4)), p(I, d(1, 4))),
    2 -> Seq(p(VIII, d(1, 4)), p(VIIb, d(1, 4)), p(VI, d(1, 4)), p(V, d(1, 4))))
}

object Boogie extends Style {
  import ScaleDegree._
  val name: String = "boogie"
  val pattern = Map(
    0 -> Seq(p(I, d(1, 8)), p(I, d(1, 8)), p(III, d(1, 8)), p(III, d(1, 8)), p(V, d(1, 8)), p(V, d(1, 8)), p(VI, d(1, 8)), p(V, d(1, 8))),
    1 -> Seq(p(I, d(1, 4)), p(null, d(1, 8)), p(III, d(1, 8)), p(null, d(1, 4)), p(V, d(1, 4))))

  val fill = Map(
    0 -> Seq(p(I, d(1, 8)), p(I, d(1, 8)), p(III, d(1, 8)), p(III, d(1, 8)), p(V, d(1, 8)), p(V, d(1, 8)), p(VI, d(1, 8)), p(V, d(1, 8))),
    1 -> Seq(p(null, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8))),
    2 -> Seq(p(VIII, d(1, 4)), p(null, d(1, 8)), p(VI, d(1, 8)), p(null, d(1, 4)), p(V, d(1, 4))))
}

object Reggae extends Style {
  import ScaleDegree._
  val name: String = "reggae"
  val pattern = Map(
    0 -> Seq(p(null, d(3, 16)), p(VIII, d(1, 16)), p(VI, d(1, 8)), p(VIII, d(1, 16)), p(null, d(1, 16)), p(null, d(3, 16)), p(VIII, d(1, 16)), p(VI, d(1, 8)), p(V, d(1, 16)), p(null, d(1, 16))))

  val fill = Map(
    0 -> Seq(p(null, d(3, 16)), p(VIII, d(1, 16)), p(VI, d(1, 8)), p(VIII, d(1, 16)), p(null, d(1, 16)), p(null, d(3, 16)), p(VIII, d(1, 16)), p(VI, d(1, 8)), p(V, d(1, 16)), p(null, d(1, 16))))
}

object Soul extends Style {
  import ScaleDegree._
  val name: String = "soul"
  val pattern = Map(
    0 -> Seq(p(I, d(1, 4)), p(null, d(1, 8)), p(I, d(1, 8, d(1, 4))), p(IIIb, d(1, 8)), p(III, d(1, 8))),
    1 -> Seq(p(I, d(1, 4)), p(null, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 4)), p(IIIb, d(1, 8)), p(III, d(1, 8))),
    2 -> Seq(p(I, d(1, 4)), p(null, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 4)), p(null, d(1, 4))),
    3 -> Seq(p(I, d(1, 4)), p(null, d(1, 8)), p(I, d(1, 8, d(1, 4))), p(null, d(1, 8)), p(I, d(1, 8))))

  val fill = Map(
    0 -> Seq(p(I, d(1, 4)), p(null, d(3, 4))))
}
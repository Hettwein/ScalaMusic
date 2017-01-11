package de.htwg.scalamusic

trait Style extends MusicConversion {
  val name: String
  //  val patternA: Map[Int, Seq[Part]]
  //  val patternB: Map[Int, Seq[Part]]

  val pattern: Map[Int, List[Part]]
  val lick: Map[Int, List[Part]]
  def groove(a: Int, b: Int, l: Int): List[Part] = pattern(a) ++ pattern(b) ++ pattern(a) ++ lick(l)

  def d(a: Int, b: Int, c: Duration = null) = Duration(a, b, c)
  def p(a: ScaleDegree.Value, b: Duration, c: Int = 0) = Part(b, a, c)

  def asLy: String = ""
  def asDSL: String = s"""style '${name}' """
}

object Rock extends Style {
  import ScaleDegree._
  val name: String = "rock"
  val pattern = Map(
    0 -> List(p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8))),
    1 -> List(p(I, d(3, 8)), p(I, d(1, 8)), p(I, d(3, 8)), p(I, d(1, 8))),
    2 -> List(p(I, d(1, 8)), p(I, d(1, 8, d(1, 8))), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8, d(1, 8))), p(I, d(1, 8))))
  //    3 -> List(p(I, d(1, 4)), p(I, d(1, 4)), p(I, d(1, 4)), p(null, d(1, 8)), p(I, d(1, 8, d(1, 8))), p(I, d(1, 8, d(1, 8))), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8))))

  val lick = Map(
    0 -> List(p(I, d(3, 8)), p(I, d(1, 8)), p(I, d(1, 2))))
}

object Funk extends Style { //http://www.bassplayer.com/styles/1181/what-the-funk/25901
  import ScaleDegree._
  val name: String = "funk"
  val pattern = Map(
    0 -> List(p(I, d(1, 8)), p(VIII, d(1, 8)), p(I, d(1, 8)), p(VIII, d(1, 8)), p(I, d(1, 8)), p(VIII, d(1, 8)), p(I, d(1, 8)), p(VIII, d(1, 8))),
    //    0 -> List(p(VIIb, d(1, 16), -1), p(I, d(1, 16)), p(null, d(1, 8)), p(I, d(1, 16)), p(null, d(1, 8)), p(I, d(1, 16)), p(null, d(1, 8)), p(I, d(1, 8)), p(IV, d(1, 8)), p(IIIb, d(1, 8))), //
    1 -> List(p(I, d(1, 4)), p(null, d(1, 8)), p(null, d(1, 16)), p(I, d(1, 16)), p(I, d(1, 8)), p(I, d(1, 8)), p(null, d(1, 4))),
    2 -> List(p(I, d(1, 16)), p(I, d(1, 16)), p(null, d(1, 8)), p(null, d(1, 8)), p(null, d(1, 16)), p(I, d(1, 16)), p(null, d(1, 2))),
    3 -> List(p(I, d(1, 8)), p(VIII, d(1, 8)), p(null, d(1, 8)), p(I, d(1, 16)), p(I, d(1, 16)), p(null, d(1, 2))),
    4 -> List(p(I, d(1, 4, d(1, 8, d(1, 16)))), p(I, d(1, 16)), p(I, d(1, 8)), p(I, d(1, 8, d(1, 4)))),
    5 -> List(p(I, d(1, 8)), p(I, d(1, 8)), p(null, d(1, 4)), p(null, d(1, 8)), p(I, d(1, 8)), p(null, d(1, 16)), p(I, d(3, 16))))

  val lick = Map(
    0 -> List(p(I, d(1, 8)), p(I, d(1, 8)), p(VIII, d(1, 16)), p(VIIb, d(1, 16)), p(V, d(1, 16)), p(I, d(1, 16)), p(null, d(1, 8)), p(null, d(1, 16)), p(IIIb, d(1, 16)), p(null, d(1, 4))),
    1 -> List(p(I, d(1, 4, d(1, 8, d(1, 16)))), p(V, d(1, 16)), p(VIIb, d(1, 16)), p(V, d(1, 16)), p(VI, d(1, 16)), p(V, d(1, 16)), p(IV, d(1, 16)), p(V, d(1, 16)), p(IIIb, d(1, 16)), p(III, d(1, 16))))
}

object Jazz extends Style {
  import ScaleDegree._
  val name: String = "jazz"
  val pattern = Map(
    0 -> List(p(I, d(1, 4)), p(I, d(1, 4)), p(I, d(1, 4)), p(I, d(1, 4))),
    1 -> List(p(I, d(1, 4)), p(I, d(1, 4)), p(V, d(1, 4)), p(V, d(1, 4))),
    2 -> List(p(I, d(1, 4)), p(II, d(1, 4)), p(III, d(1, 4)), p(I, d(1, 4))),
    3 -> List(p(I, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4))),
    4 -> List(p(I, d(1, 4)), p(II, d(1, 4)), p(I, d(1, 4)), p(V, d(1, 4))),
    5 -> List(p(I, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4)), p(I, d(1, 4))),
    6 -> List(p(VIII, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4)), p(I, d(1, 4))),
    7 -> List(p(VIII, d(1, 4)), p(VIII, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4))),
    8 -> List(p(I, d(1, 4)), p(II, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4))),
    9 -> List(p(VIII, d(1, 4)), p(VII, d(1, 4)), p(VI, d(1, 4)), p(V, d(1, 4))),
    10 -> List(p(I, d(1, 4)), p(V, d(1, 4)), p(VIII, d(1, 4)), p(V, d(1, 4))),
    11 -> List(p(IV, d(1, 4)), p(III, d(1, 4)), p(II, d(1, 4)), p(I, d(1, 4))),
    12 -> List(p(I, d(1, 4)), p(I, d(1, 4)), p(II, d(1, 4)), p(I, d(1, 4))),
    13 -> List(p(I, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4)), p(VIII, d(1, 4))),
    14 -> List(p(I, d(1, 4)), p(V, d(1, 4)), p(VII, d(1, 4)), p(VIII, d(1, 4))),
    15 -> List(p(VIII, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4))),
    16 -> List(p(I, d(1, 4)), p(I, d(1, 4)), p(V, d(1, 4)), p(I, d(1, 4))),
    17 -> List(p(VIII, d(1, 4)), p(V, d(1, 4)), p(I, d(1, 4)), p(III, d(1, 4))))

  val lick = Map(
    0 -> List(p(I, d(1, 4)), p(I, d(1, 4)), p(I, d(1, 4)), p(I, d(1, 4))),
    1 -> List(p(I, d(1, 4)), p(I, d(1, 4)), p(V, d(1, 4)), p(V, d(1, 4))),
    2 -> List(p(I, d(1, 4)), p(II, d(1, 4)), p(III, d(1, 4)), p(I, d(1, 4))),
    3 -> List(p(I, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4))),
    4 -> List(p(I, d(1, 4)), p(II, d(1, 4)), p(I, d(1, 4)), p(V, d(1, 4))),
    5 -> List(p(I, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4)), p(I, d(1, 4))),
    6 -> List(p(VIII, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4)), p(I, d(1, 4))),
    7 -> List(p(VIII, d(1, 4)), p(VIII, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4))),
    8 -> List(p(I, d(1, 4)), p(II, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4))),
    9 -> List(p(VIII, d(1, 4)), p(VII, d(1, 4)), p(VI, d(1, 4)), p(V, d(1, 4))),
    10 -> List(p(I, d(1, 4)), p(V, d(1, 4)), p(VIII, d(1, 4)), p(V, d(1, 4))),
    11 -> List(p(IV, d(1, 4)), p(III, d(1, 4)), p(II, d(1, 4)), p(I, d(1, 4))),
    12 -> List(p(I, d(1, 4)), p(I, d(1, 4)), p(II, d(1, 4)), p(I, d(1, 4))),
    13 -> List(p(I, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4)), p(VIII, d(1, 4))),
    14 -> List(p(I, d(1, 4)), p(V, d(1, 4)), p(VII, d(1, 4)), p(VIII, d(1, 4))),
    15 -> List(p(VIII, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4))),
    16 -> List(p(I, d(1, 4)), p(I, d(1, 4)), p(V, d(1, 4)), p(I, d(1, 4))),
    17 -> List(p(VIII, d(1, 4)), p(V, d(1, 4)), p(I, d(1, 4)), p(III, d(1, 4))))
}

object Swing extends Style {
  import ScaleDegree._
  val name: String = "swing"
  val pattern = Map(
    0 -> List(p(I, d(1, 4)), p(null, d(1, 6)), p(I, d(1, 12)), p(I, d(1, 4)), p(null, d(1, 6)), p(I, d(1, 12))))

  val lick = Map(
    0 -> List(p(I, d(1, 4)), p(null, d(1, 6)), p(I, d(1, 12)), p(I, d(1, 6)), p(I, d(1, 12)), p(II, d(1, 6)), p(III, d(1, 12))))

  override def groove(a: Int, b: Int, l: Int): List[Part] = pattern(a) ++ lick(l) ++ pattern(b) ++ lick(l)
}

object SlowRock extends Style {
  import ScaleDegree._
  val name: String = "slowRock"
  val pattern = Map(
    0 -> List(p(I, d(1, 4)), p(I, d(1, 6)), p(III, d(1, 12)), p(V, d(1, 4)), p(III, d(1, 6)), p(V, d(1, 12))),
    1 -> List(p(I, d(3, 4)), p(I, d(1, 4))))

  val lick = Map(
    0 -> List(p(I, d(1, 4)), p(I, d(1, 6)), p(III, d(1, 12)), p(V, d(1, 4)), p(III, d(1, 6)), p(V, d(1, 12))),
    1 -> List(p(I, d(3, 4)), p(I, d(1, 4))),
    2 -> List(p(VIII, d(1, 4)), p(VIIb, d(1, 4)), p(VI, d(1, 4)), p(V, d(1, 4))))
}

object Boogie extends Style {
  import ScaleDegree._
  val name: String = "boogie"
  val pattern = Map(
    0 -> List(p(I, d(1, 8)), p(I, d(1, 8)), p(III, d(1, 8)), p(III, d(1, 8)), p(V, d(1, 8)), p(V, d(1, 8)), p(VI, d(1, 8)), p(V, d(1, 8))),
    1 -> List(p(I, d(1, 4)), p(null, d(1, 8)), p(III, d(1, 8)), p(null, d(1, 4)), p(V, d(1, 4))))

  val lick = Map(
    0 -> List(p(I, d(1, 8)), p(I, d(1, 8)), p(III, d(1, 8)), p(III, d(1, 8)), p(V, d(1, 8)), p(V, d(1, 8)), p(VI, d(1, 8)), p(V, d(1, 8))),
    1 -> List(p(null, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 8))),
    2 -> List(p(VIII, d(1, 4)), p(null, d(1, 8)), p(VI, d(1, 8)), p(null, d(1, 4)), p(V, d(1, 4))))
}

object Reggae extends Style {
  import ScaleDegree._
  val name: String = "reggae"
  val pattern = Map(
    0 -> List(p(null, d(3, 16)), p(VIII, d(1, 16)), p(VI, d(1, 8)), p(VIII, d(1, 16)), p(null, d(1, 16)), p(null, d(3, 16)), p(VIII, d(1, 16)), p(VI, d(1, 8)), p(V, d(1, 16)), p(null, d(1, 16))))

  val lick = Map(
    0 -> List(p(null, d(3, 16)), p(VIII, d(1, 16)), p(VI, d(1, 8)), p(VIII, d(1, 16)), p(null, d(1, 16)), p(null, d(3, 16)), p(VIII, d(1, 16)), p(VI, d(1, 8)), p(V, d(1, 16)), p(null, d(1, 16))))
}

object Ska extends Style {
  import ScaleDegree._
  val name: String = "ska"
  val pattern = Map(
    0 -> List(p(I, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4)), p(III, d(1, 8)), p(I, d(1, 8))),
    1 -> List(p(I, d(1, 4)), p(III, d(1, 4)), p(V, d(1, 4)), p(I, d(1, 4))))

  val lick = Map(
    0 -> List(p(VIII, d(1, 4)), p(VIII, d(1, 4)), p(VII, d(1, 4)), p(VI, d(1, 4))))
}

object Soul extends Style {
  import ScaleDegree._
  val name: String = "soul"
  val pattern = Map(
    0 -> List(p(I, d(1, 4)), p(null, d(1, 8)), p(I, d(1, 8, d(1, 4))), p(IIIb, d(1, 8)), p(IVb, d(1, 8))),
    1 -> List(p(I, d(1, 4)), p(null, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 4)), p(IIIb, d(1, 8)), p(IVb, d(1, 8))),
    2 -> List(p(I, d(1, 4)), p(null, d(1, 8)), p(I, d(1, 8)), p(I, d(1, 4)), p(null, d(1, 4))),
    3 -> List(p(I, d(1, 4)), p(null, d(1, 8)), p(I, d(1, 8, d(1, 4))), p(null, d(1, 8)), p(I, d(1, 8))))

  val lick = Map(
    0 -> List(p(I, d(1, 4)), p(null, d(3, 4))))
}
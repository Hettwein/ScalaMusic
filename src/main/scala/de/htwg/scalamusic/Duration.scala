package de.htwg.scalamusic

case class Duration(numerator: Int = 1, denominator: Int = 4, tied: Duration = null) extends MusicConversion {

  def sum(b: Duration): Duration = {
    val d = lcm(denominator, b.denominator)
    val n = d / denominator * numerator + d / b.denominator * b.numerator
    Duration(n / gcd(d, n), d / gcd(d, n))
  }

  def mul(b: Duration): Duration = {
    val n = numerator * b.numerator
    val d = denominator * b.denominator
    Duration(n / gcd(d, n), d / gcd(d, n))
  }

  def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
  def lcm(a: Int, b: Int) = (a * b).abs / gcd(a, b)

  def getValue(): Double = numerator.toDouble / denominator.toDouble + (if (tied != null) getTied.foldLeft(Duration(0, 1))((n, b) => n.sum(b)).getValue() else 0)

  def getTied(): List[Duration] = {
    if (tied != null) {
      List(Duration(tied.numerator, tied.denominator)) ++ tied.getTied()
    } else {
      List()
    }
  }

  def asLy: String = s"""${if (numerator == 1) denominator else denominator / 2 + "."}"""
  def asDSL: String = s"""${if (numerator == 1) denominator else denominator / 2 + "."}"""
}
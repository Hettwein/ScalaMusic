package de.htwg.scalamusic

case class Beat(numerator: Int = 1, denominator: Int = 4, tied: Seq[Beat] = Seq()) extends MusicConversion {

  def sum(b: Beat): Beat = {
    val d = lcm(denominator, b.denominator)
    val n = d / denominator * numerator + d / b.denominator * b.numerator
    Beat(n / gcd(d, n), d / gcd(d, n))
  }

  def mul(b: Beat): Beat = {
    val n = numerator * b.numerator
    val d = denominator * b.denominator
    Beat(n / gcd(d, n), d / gcd(d, n))
  }

  def gcd(a: Int, b: Int): Int = if (b == 0) a.abs else gcd(b, a % b)
  def lcm(a: Int, b: Int) = (a * b).abs / gcd(a, b)

  def getValue(): Double = numerator.toDouble / denominator.toDouble + (if(!tied.isEmpty) tied.foldLeft(Beat(0, 1))((n, b) => n.sum(b)).getValue() else 0)
  
  def asLy: String = ""
  def asDSL: String = ""
}
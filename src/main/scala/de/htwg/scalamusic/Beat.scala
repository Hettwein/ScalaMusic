package de.htwg.scalamusic

case class Beat(numerator: Int = 1, denominator: Int = 4) extends MusicConversion {

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

  def getValue(): Double = numerator.toDouble / denominator.toDouble
  
//  def getRandom(min: Beat, max: Beat): Beat = {
////    (Math.random() * 1 + time.denominator).floor.toInt
////    val rand = Math.random() * max.getValue() + min.getValue()
//    val rDenom = (Math.random() * min.denominator + max.denominator).floor.toInt
//    val bl = Map(
//        0 -> min,
//        1 -> min.sum(min.mul(Beat(1, 2))),
//        2 -> ???)
//        
//  }
}
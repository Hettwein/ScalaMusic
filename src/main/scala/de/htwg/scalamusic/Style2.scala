package de.htwg.scalamusic

trait Style2 extends MusicConversion {

  val name: String
  val pattern: Map[Int, Seq[(ScaleDegree.Value, Beat)]]
  val fill: Map[Int, Seq[(ScaleDegree.Value, Beat)]]

  def asLy: String = ""
  def asDSL: String = s"""style '${name}' """
}

case class Rock() extends Style2 {
  val name: String = "rock"
//  val asdf: Seq[(ScaleDegree.Value, Beat)] = Seq((ScaleDegree.I, Beat(1,8)), (null, Beat(1, 8)))
  
  val pattern = Map(
    0 -> Seq((ScaleDegree.I, Beat(1, 8))),
    1 -> Seq((ScaleDegree.I, Beat(3, 8)), (ScaleDegree.I, Beat(1, 8))))
    
  val fill = Map(
    0 -> Seq((ScaleDegree.I, Beat(3, 8)), (ScaleDegree.I, Beat(1, 8)), (ScaleDegree.I, Beat(1, 2))))
}
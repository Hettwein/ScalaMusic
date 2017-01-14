package de.htwg.scalamusic

case class Tuplet(number: Int = 3, elements: Seq[MusicElement]) extends MusicElement {

  val duration: Duration = elements.foldLeft(Duration(0, 1))((n, m) => n.sum(m.duration)).mul(Duration(2, number))
  override def asLy(): String = s"""\\tuplet ${number}/2 { ${elements.foldLeft("")((s, m) => s + m.asLy + " ")}}"""
  override def asDSL(): String = s"""( ${elements.foldLeft("")((s, m) => s + m.asDSL + " ")})${number}"""
}

//  private val group: Seq[Seq[MusicElement]] = {
//    for(i <- 0 until elements.size) yield {
//      var n1 = 0.0
//    	var n2 = 0.0
//      val s = elements.dropWhile {x => n1 += x.duration.getValue(); println(n1); n1 != 1.5 * (i+1.0) && n1 != 0.75 * (i+1.0) && n1 != 0.375 * (i+1.0) && n1 != 0.1875 * (i+1.0)}
//      println(s.size)
//      s.takeWhile { x => n2 += x.duration.getValue(); n2 != 1.5 && n2 != 0.75 && n2 != 0.375 && n2 != 0.1875}
//    }
////    elements.filter { x => ??? }
////    var l: Seq[Seq[MusicElement]] = Seq(Seq())
////    var i = 0
////    elements.foldLeft(Duration(0, 1))((n, m) => {val s = n.sum(m.duration); val v = s.getValue(); println(v); if(v == 1.5 || v == 0.75 || v == 0.375 || v == 0.1875) {i += 1; l = l :+ Seq()}; l(i) = l(i) :+ m; s})
////    l
//  }
//  override def asLy(): String = group.foldLeft("")((s, m) => s + s"""\\tuplet ${number}/2 { ${m.foldLeft("")((s1, m1) => s1 + m1.asLy + " ")}}""")
//  override def asDSL(): String = group.foldLeft("")((s, m) => s + s"""( ${elements.foldLeft("")((s, m) => s + m.asDSL + " ")})${number}""")
package de.htwg.scalamusic

case class Repetition(measures: Seq[Measure], alternatives: Seq[Seq[Measure]] = Seq()) extends MusicSegment {
  
  val music: Seq[Measure] = measures++alternatives.map { x => x }.flatten
//		  val music: Seq[Measure] = alternatives.map { x => measures++x }.flatten

  override def asLy(): String = s"""\\repeat volta ${"2"} {${measures.foldLeft("")((s, m) => s + m.asLy + " ")}}
                                    |      \\alternative {
                                    |        ${alternatives.foldLeft("")((s, m) => s + "{" + m.foldLeft("")((s, m) => s + m.asLy + " ") + "}")}
                                    |      }"""
  override def asDSL(): String = s"""|: ${measures.foldLeft("")((s, m) => s + m.asDSL + " ")}${alternatives.foldLeft("")((s, m) => s + "[" + m.foldLeft("")((s, m) => s + m.asDSL + " ") + "]")}:|"""
}
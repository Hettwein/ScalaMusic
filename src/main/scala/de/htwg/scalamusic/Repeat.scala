package de.htwg.scalamusic

case class Repeat(music: Seq[MusicSegment], alternatives: Seq[Seq[Measure]] = Seq()) extends MusicSegment {

  override def asLy(): String = s"""\\repeat volta ${"2"} {${music.foldLeft("")((s, m) => s + m.asLy + " ")}}
                                    |      \\alternative {
                                    |        ${alternatives.foldLeft("")((s, m) => s + "{" + m.foldLeft("")((s, m) => s + m.asLy + " ") + "}")}
                                    |      }"""
  override def asDSL(): String = s"""|: ${music.foldLeft("")((s, m) => s + m.asDSL + " ")}${alternatives.foldLeft("")((s, m) => s + "[" + m.foldLeft("")((s, m) => s + m.asDSL + " ") + "]")}:|"""
}
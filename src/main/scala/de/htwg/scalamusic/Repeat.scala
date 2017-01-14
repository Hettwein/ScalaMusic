package de.htwg.scalamusic

class Repeat(val music: Seq[Repeat] = Seq(), val alternatives: Seq[Seq[Measure]] = Seq()) extends MusicConversion {

  override def asLy(): String = s"""\\repeat volta ${if (alternatives.size < 2) "2" else alternatives.size} {${music.foldLeft("")((s, m) => s + m.asLy + " ")}}
                                    |      \\alternative {
                                    |        ${alternatives.foldLeft("")((s, m) => s + "{" + m.foldLeft("")((s, m) => s + m.asLy + " ") + "}")}
                                    |      }"""
  override def asDSL(): String = s"""|: ${music.foldLeft("")((s, m) => s + m.asDSL + " ")}${alternatives.foldLeft("")((s, m) => s + "[" + m.foldLeft("")((s, m) => s + m.asDSL + " ") + "]")}:|"""
}
package de.htwg.scalamusic.music

case class Voice(var music: Seq[MusicElement] = Seq()) extends MusicSequence[MusicElement] {
  override def asLy: String = s"""|
                                  |    { ${music.foldLeft("")((s, m) => s + m.asLy + " ")}}""".stripMargin
  override def asDSL: String = s"""{ ${music.foldLeft("")((s, m) => s + m.asDSL + " ")}}"""
}
package de.htwg.scalamusic.music

case class Staff(music: Seq[Voice] = Seq()) extends MusicConversion {
  override def asLy: String = s"""|
                              |  <<
                              |    ${music.foldLeft("")((s, m) => s + m.asLy + " ")}
                              |  >>""".stripMargin
  override def asDSL: String = s"""( ${music.foldLeft("")((s, m) => s + m.asDSL + " ")})"""
}
package de.htwg.scalamusic.music

case class Score(tempo: Double = 115, timeSignature: TimeSignature = TimeSignature(), var music: Seq[Staff] = Seq()) extends MusicComposite[Staff] {
  override def asLy: String = s"""<<
                              |  ${timeSignature.asLy}${music.foldLeft("")((s, m) => s + m.asLy + " ")}
                              |>>""".stripMargin
  override def asDSL: String = s"""<< ${music.foldLeft("")((s, m) => s + m.asDSL + " ")}>>"""
}
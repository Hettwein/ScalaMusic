package de.htwg.scalamusic.music

case class Score(tempo: Double = 115, timeSignature: TimeSignature = TimeSignature(), var music: Seq[Staff] = Seq()) extends MusicComposite[Staff] {
  override def asLy: String = s"""\\score {
                              |<<
                              |  ${timeSignature.asLy}${music.foldLeft("")((s, m) => s + m.asLy + " ")}
                              |>>
                              |\\layout { }
                              |\\midi {
                              |  \\context {
                              |    \\Staff
                              |    \\remove "Staff_performer"
                              |  }
                              |  \\context {
                              |    \\Voice
                              |    \\consists "Staff_performer"
                              |  }
                              |  \\tempo ${timeSignature.denominator} = ${tempo.toInt}
                              |}
                              |}""".stripMargin
  override def asDSL: String = s"""( ${timeSignature.asDSL} ${music.foldLeft("")((s, m) => s + m.asDSL + " ")})"""
}
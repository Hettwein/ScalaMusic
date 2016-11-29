package de.htwg.scalamusic

case class Score(music: Seq[Staff] = Seq()) extends MusicConversion {
  override def asLy: String = s"""\\score {
                              |<<
                              |  ${music.foldLeft("")((s, m) => s + m.asLy + " ")}
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
                              |}
                              |}""".stripMargin
  override def asDSL: String = s"""(${music.foldLeft("")((s, m) => s + m.asDSL + " ")})"""
}
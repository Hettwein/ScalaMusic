package de.htwg.scalamusic

case class Score(style: Style = null, music: Seq[Staff] = Seq()) extends MusicConversion {
  override def asLy: String = s"""\\score {
                              |<<
                              |  ${music.foldLeft("")((s, m) => s + m.asLy + " ")}
                              |>>
                              |\\layout { }
                              |}
                              |
                              |\\score {
                              |\\unfoldRepeats 
                              |<<
                              |  ${music.foldLeft("")((s, m) => s + m.asLy + " ")}
                              |>>
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
  override def asDSL: String = s"""( ${if (style != null) style.asDSL else ""}${music.foldLeft("")((s, m) => s + m.asDSL + " ")})"""
}
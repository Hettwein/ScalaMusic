package de.htwg.scalamusic

case class Score(style: Style.Value, music: Seq[Staff] = Seq()) extends MusicConversion {
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
  override def asDSL: String = s"""( style '${style}' ${music.foldLeft("")((s, m) => s + m.asDSL + " ")})"""
}

object Style extends Enumeration {
  type Style = Value
  val jazz, rock, funk, pop, blues, blues2, random, randomTriplets, beat, follow = Value // look for proper styles

  def apply(s: String): Style = withName(s)
}
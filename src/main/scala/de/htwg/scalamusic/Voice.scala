package de.htwg.scalamusic

class Voice(music: Seq[MusicSegment] = Seq(), instrument: String = "") extends MusicConversion {
  //  val measures: Seq[Measure] = music.foldLeft(Seq(): Seq[Measure])((s, m) => s ++ (if (m.isInstanceOf[Measure]) Seq(m.asInstanceOf[Measure]) else m.asInstanceOf[Repeat].music))
  override def asLy: String = s"""|
			|    { ${if (instrument != "") "\\set Voice.midiInstrument = #\"" + instrument + "\" " else ""}${music.foldLeft("")((s, m) => s + m.asLy + " ")}}""".stripMargin
  override def asDSL: String = s"""( ${music.foldLeft("")((s, m) => s + m.asDSL + " ")})"""
}

//class Voice(music: Seq[MusicSegment] = Seq(), instrument: String = "", repeat: Int = 1, alternatives: Seq[Seq[Measure]] = Seq()) extends MusicConversion {
//  override def asLy(): String = s"""\\repeat volta ${"2"} {${music.foldLeft("")((s, m) => s + m.asLy + " ")}}
//                                    |      \\alternative {
//                                    |        ${alternatives.foldLeft("")((s, m) => s + "{" + m.foldLeft("")((s, m) => s + m.asLy + " ") + "}")}
//                                    |      }"""
//  override def asDSL(): String = s"""|: ${music.foldLeft("")((s, m) => s + m.asDSL + " ")}${alternatives.foldLeft("")((s, m) => s + "[" + m.foldLeft("")((s, m) => s + m.asDSL + " ") + "]")}:|"""
//}
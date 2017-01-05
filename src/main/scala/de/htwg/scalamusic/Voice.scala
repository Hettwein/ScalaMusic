package de.htwg.scalamusic

class Voice(music: Seq[Repeat] = Seq(), instrument: String = "") extends MusicConversion {

  override def asLy: String = s"""|
			|    { ${if (instrument != "") "\\set Voice.midiInstrument = #\"" + instrument + "\" " else ""}${music.foldLeft("")((s, m) => s + m.asLy + " ")}}""".stripMargin
  override def asDSL: String = s"""( ${music.foldLeft("")((s, m) => s + m.asDSL + " ")})"""
}
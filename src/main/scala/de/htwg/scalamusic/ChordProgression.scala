package de.htwg.scalamusic

case class ChordProgression(music: Seq[Measure] = Seq(), instrument: String = "") extends Voice(music, instrument) {
  override def asLy: String = s"""\\new ChordNames { ${if (instrument != "") "\\set Voice.midiInstrument = #\"" + instrument + "\" " else ""}${music.foldLeft("")((s, m) => s + m.asLy + " ")}}"""
  override def asDSL: String = s"""chords( ${music.foldLeft("")((s, m) => s + m.asDSL + " ")})"""
}
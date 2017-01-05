package de.htwg.scalamusic

case class ChordProgression(music: Seq[Repeat] = Seq(), instrument: String = "") extends Voice(music, instrument) {
  //  val measures: Seq[Measure] = music.foldLeft(Seq(): Seq[Measure])((s, m) =>  s++(if (m.isInstanceOf[Measure]) Seq(m.asInstanceOf[Measure]) else m.asInstanceOf[Repetition].music))
  override def asLy: String = s"""\\new ChordNames { ${if (instrument != "") "\\set Voice.midiInstrument = #\"" + instrument + "\" " else ""}${music.foldLeft("")((s, m) => s + m.asLy + " ")}}"""
  override def asDSL: String = s"""chords( ${music.foldLeft("")((s, m) => s + m.asDSL + " ")})"""
}
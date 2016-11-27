package de.htwg.scalamusic.music

case class ChordProgression(var music: Seq[Chord] = Seq()) extends MusicSequence[Chord] {
  override def asLy: String = s"""\\new ChordNames { \\set ChordNames.midiInstrument = #"string ensemble 1" ${music.foldLeft("")((s, m) => s + m.asLy + " ")}}"""
  override def asDSL: String = s"""chords( ${music.foldLeft("")((s, m) => s + m.asDSL + " ")})"""
}
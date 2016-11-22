package de.htwg.scalamusic.music

case class ChordProgression(var music: Seq[Chord] = Seq()) extends MusicSequence[Chord] {
  override def asLy: String = s"""\\new ChordNames { ${music.foldLeft("")((s, m) => s + m.asLy + " ")}}"""
  override def asDSL: String = s"""{ ${music.foldLeft("")((s, m) => s + m.asDSL + " ")}}"""
}
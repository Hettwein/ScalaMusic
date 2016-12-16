package de.htwg.scalamusic

import scala.language.postfixOps

class BassGenerator(val score: Score) {

  def generate(): Score = {
    val style = score.style
    val chords = extractChords().measures

    var t = chords(0).timeSignature
    var k = chords(0).key

    val bassline = for (i <- 0 until chords.size) yield { // iterate over measures
      val bar = chords(i)
      if (bar.keyChange) k = bar.key
      if (bar.timeChange) t = bar.timeSignature
      val music: Seq[MusicElement] = applyPattern(bar.music.asInstanceOf[Seq[Chord]], t, getPattern(style, i + 1))
      Measure(timeSignature = t, key = k, clef = Clef.bass, timeChange = bar.timeChange, clefChange = (bar == chords(0)), keyChange = bar.keyChange, music = music)
    }
    score.copy(music = Seq(Staff(score.music(0).music :+ new Voice(bassline, "electric bass (finger)"))))
  }

  def getPattern(style: Style, count: Int): Seq[(ScaleDegree.Value, Beat)] = {
    style match {
      case Rock =>
        if (count % 4 != 0) {
          Rock.pattern(Math.random().round.toInt * (Rock.pattern.size - 1))
        } else {
          Rock.fill(Math.random().round.toInt * (Rock.fill.size - 1))
        }
      case Jazz =>
        if (count % 4 != 0) {
          Jazz.pattern(Math.random().round.toInt * (Jazz.pattern.size - 1))
        } else {
          Jazz.fill(Math.random().round.toInt * (Jazz.fill.size - 1))
        }
      case Funk =>
        if (count % 4 != 0) {
          Funk.pattern(Math.random().round.toInt * (Funk.pattern.size - 1))
        } else {
          Funk.fill(Math.random().round.toInt * (Funk.fill.size - 1))
        }
    }
  }

  def applyPattern(chords: Seq[Chord], time: TimeSignature, pattern: Seq[(ScaleDegree.Value, Beat)]): Seq[MusicElement] = {
    val t = time.numerator.toDouble / time.denominator.toDouble
    val duration: Beat = pattern.foldLeft(Beat(0, 1))((n, m) => n.sum(m._2))
    var d = 0.0
    var n = 0
    var c = 0.0
    for (i <- 0 until (t / duration.getValue * pattern.size).toInt; if (d < 1.0)) yield {
      val p = pattern(i % pattern.size)
      if (c >= chords(n).duration.getValue()) {
        c = c - chords(n).duration.getValue()
        n += 1
      }
      val chord = chords(n)
      d += p._2.getValue()
      c += p._2.getValue()
      if (p._1 != null) Note(chord.getScale.getDegreePitch(p._1)-, p._2) else Rest(p._2)
    }
  }

  def extractChords(): ChordProgression = {
    score.music.foreach { x => x.music.foreach { m => if (m.isInstanceOf[ChordProgression]) { return m.asInstanceOf[ChordProgression] } } }
    ChordProgression()
  }

}
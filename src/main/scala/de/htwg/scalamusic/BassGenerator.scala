package de.htwg.scalamusic

import scala.language.postfixOps

class BassGenerator(val score: Score) {

  def generate(): Score = {
//    val style = score.style
//    val chords = extractChords().music

//    var t = chords(0).timeSignature
//    var k = chords(0).key

    val bassline = generate(score.style, extractChords().music, TimeSignature(), MajorScale(Pitch()))
    score.copy(music = Seq(Staff(score.music(0).music :+ new Voice(bassline, "electric bass (finger)"))))
  }

  def generate(style: Style, segments: Seq[MusicSegment], time: TimeSignature, key: Key, n: Int = 0): Seq[MusicSegment] = {
    for (i <- 0 until segments.size) yield { // iterate over measures
      var k = key
      var t = time
      if (segments(i).isInstanceOf[Measure]) {
        val bar = segments(i).asInstanceOf[Measure]
        if (bar.keyChange) k = bar.key
        if (bar.timeChange) t = bar.timeSignature
        val music: Seq[MusicElement] = applyPattern(bar.music.asInstanceOf[Seq[Chord]], t, getPattern(style, if(n == 0) i + 1 else n))////
        Measure(timeSignature = t, key = k, clef = Clef.bass, timeChange = bar.timeChange, clefChange = (bar == segments(0)), keyChange = bar.keyChange, music = music)
      } else {
        val rep = segments(i).asInstanceOf[Repeat]
        Repeat(generate(style, rep.music, time, key), rep.alternatives.map { x => generate(style, x, time, key, 4).asInstanceOf[Seq[Measure]] })
      }
    }
  }

  def getPattern(style: Style, count: Int): Seq[(ScaleDegree.Value, Beat)] = {
    if (count % 4 != 0) {
      style.pattern(Math.random().round.toInt * (style.pattern.size - 1))
    } else {
      style.fill(Math.random().round.toInt * (style.fill.size - 1))
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
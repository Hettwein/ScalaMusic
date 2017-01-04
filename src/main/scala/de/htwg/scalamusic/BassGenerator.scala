package de.htwg.scalamusic

import scala.language.postfixOps

class BassGenerator(val score: Score) {

  var last: (Double, Seq[(ScaleDegree.Value, Duration)]) = (0, Seq())

  def generate(): Score = {
    val bassline = generate(score.style, extractChords().music)
    score.copy(music = Seq(Staff(score.music(0).music :+ new Voice(bassline, "electric bass (finger)"))))
  }

  def generate(style: Style, segments: Seq[MusicSegment], n: Int = 0): Seq[MusicSegment] = {
    val a = Math.random()
    val b = Math.random()
    for (i <- 0 until segments.size) yield { // iterate over measures/ repeats
      if (segments(i).isInstanceOf[Measure]) {
        val bar = segments(i).asInstanceOf[Measure]
        val d = bar.music.foldLeft(Duration(0, 1))((n, m) => n.sum(m.duration)).getValue()
        val music: Seq[MusicElement] = applyPattern(bar.music /*.asInstanceOf[Seq[Chord]]*/ , bar.timeSignature, getPattern(style, a, b, if (n == 0) i + 1 else n), d)
        bar.copy(clef = Clef.bass, music = music, clefChange = segments(0) == bar)
      } else {
        val rep = segments(i).asInstanceOf[Repeat]
        Repeat(generate(style, rep.music), rep.alternatives.map { x => generate(style, x, 4).asInstanceOf[Seq[Measure]] })
      }
    }
  }

  def getPattern(style: Style, partA: Double, partB: Double, count: Int): Seq[(ScaleDegree.Value, Duration)] = {
    if (last._1 < last._2.size) {
      last._2.drop(last._1.toInt)
    } else {
      if (count % 4 != 0) {
        if (count % 2 != 0) {
          style.pattern((partA * (style.pattern.size - 1)).round.toInt)
        } else {
          style.pattern((partB * (style.pattern.size - 1)).round.toInt)
        }
      } else {
        style.fill((Math.random() * (style.fill.size - 1)).round.toInt)
      }
    }
  }

  def applyPattern(chords: Seq[MusicElement], time: TimeSignature, pattern: Seq[(ScaleDegree.Value, Duration)], measureLength: Double): Seq[MusicElement] = {
    val duration = pattern.foldLeft(0.0)((n, m) => n + m._2.getValue())
    var d = 0.0
    var n = 0
    var c = 0.0
    last = (measureLength / duration * pattern.size, pattern)
    var triplets = false
    for (i <- 0 until (measureLength / duration * pattern.size).toInt; if (d < measureLength && ((pattern(i % pattern.size)._2.denominator % 3 == 0 && !triplets) || pattern(i % pattern.size)._2.denominator % 3 != 0))) yield { ////
      val p = pattern(i % pattern.size)
      c = BigDecimal(c).setScale(7, BigDecimal.RoundingMode.HALF_UP).toDouble //  
      if (c >= chords(n).duration.getValue()) {
        c = c - chords(n).duration.getValue()
        n += 1
      }
      val chord: Chord = if (chords(n).isInstanceOf[Chord]) chords(n).asInstanceOf[Chord] else null
      if (chord != null) {
        if (p._2.denominator % 3 == 0 && !triplets) { // triplets
          triplets = true
          var j = i
          var m: Seq[MusicElement] = Seq()
          while (pattern(j % pattern.size)._2.denominator % 3 == 0) { // group triplets
            val e = pattern(j % pattern.size)
            d += e._2.getValue()
            c += e._2.getValue()
            if (c >= chords(n).duration.getValue()) {
              c = c - chords(n).duration.getValue()
              n += 1
            }
            val chord: Chord = chords(n).asInstanceOf[Chord]
            val b = Duration(e._2.numerator, (e._2.denominator * 2.0 / 3.0).toInt)
            if (e._1 != null) m = m :+ Note(chord.getScale.getDegreePitch(e._1)-, b) else m = m :+ Rest(b)
            j += 1
          }
          Tuplet(3, m)
        } else {
          triplets = false
          d += p._2.getValue()
          c += p._2.getValue()
          if (p._1 != null && chord != null) Note(chord.getScale.getDegreePitch(p._1)-, p._2) else Rest(p._2)
        }
      } else {//no chord ?
        d += chords(n).duration.getValue()
        c += chords(n).duration.getValue()
        chords(n)
      }
    }
  }

  def extractChords(): ChordProgression = {
    score.music.foreach { x => x.music.foreach { m => if (m.isInstanceOf[ChordProgression]) { return m.asInstanceOf[ChordProgression] } } }
    ChordProgression()
  }

}
package de.htwg.scalamusic

import scala.language.postfixOps

class BasslineGenerator(val score: Score) {

  var last: Seq[Shape] = Seq()

  def generate(): Score = {
    val bassline = generate(score.style, extractChords().music)
    if (!bassline.isEmpty) {
      score.copy(music = score.music :+ Staff(Seq(new Voice(bassline, "electric bass (finger)"))))
    } else {
      score
    }
  }

  def generate(style: Style, segments: Seq[Repeat]): Seq[Repeat] = {
    if (style == null) { println("No style selected!"); return Seq() }
    if (segments.isEmpty) { println("No chords given!"); return Seq() }
    val a = Math.random()
    val b = Math.random()
    for (i <- 0 until segments.size) yield { // iterate over measures/ repeats
      if (segments(i).isInstanceOf[Measure]) {
        val bar = segments(i).asInstanceOf[Measure]
        val d = bar.elements.foldLeft(Duration(0, 1))((n, m) => n.sum(m.duration)).getValue()
        if (bar.partial != null) {
          bar.copy(clef = Clef.bass, elements = Seq(Rest(bar.partial)), clefChange = segments(0) == bar)
        } else {
          bar.copy(clef = Clef.bass, elements = applyPattern(bar.elements, bar.timeSignature, getPattern(style, a, b), d), clefChange = segments(0) == bar)
        }
      } else {
        val rep = segments(i).asInstanceOf[Repeat]
        new Repeat(generate(style, rep.music), rep.alternatives.map { x => if (last.size == 0) generate(style, rep.music ++ x).asInstanceOf[Seq[Measure]].drop(rep.music.size) else generate(style, x).asInstanceOf[Seq[Measure]] })
      }
    }
  }

  def getPattern(style: Style, partA: Double, partB: Double): Seq[Shape] = {
    if (last.size > 0) {
      last
    } else {
      style.riff((partA * (style.pattern.size - 1)).round.toInt, (partB * (style.pattern.size - 1)).round.toInt, (Math.random() * (style.lick.size - 1)).round.toInt)
    }
  }

  def applyPattern(chords: Seq[MusicElement], time: TimeSignature, pattern: Seq[Shape], measureLength: Double): Seq[MusicElement] = {
    val duration = pattern.foldLeft(0.0)((n, m) => n + m.duration.getValue())
    var d = 0.0 //position in measure
    var n = 0 //current chord
    var c = 0.0 //position in chord
    var j = 0 //already set triplets
    last = pattern
    for (i <- 0 until pattern.size; if (d < measureLength && ((pattern(i % pattern.size).duration.denominator % 3 == 0 && i >= j) || pattern(i % pattern.size).duration.denominator % 3 != 0))) yield { ////
      val p = pattern(i % pattern.size)
      if (c >= chords(n).duration.getValue()) {
        c = c - chords(n).duration.getValue()
        n += 1
      }
      val chord: Chord = if (chords(n).isInstanceOf[Chord]) chords(n).asInstanceOf[Chord] else null
      if (chord != null) {
        if (p.duration.denominator % 3 == 0) { // triplets
          j = i
          var m: Seq[MusicElement] = Seq()
          val o = c
          while (pattern(j % pattern.size).duration.denominator % 3 == 0 && (c - o != 1.0 && c - o != 0.5 && c - o != 0.25 && c - o != 0.125 && c - o != 0.0625 && c - o != 0.03125)) { // group triplets
            val e = pattern(j % pattern.size)
            d += e.duration.getValue()
            c += e.duration.getValue()
            c = BigDecimal(c).setScale(7, BigDecimal.RoundingMode.HALF_UP).toDouble //
            if (c > chords(n).duration.getValue()) {
              c = c - chords(n).duration.getValue()
              n += 1
            }
            val chord: Chord = chords(n).asInstanceOf[Chord]
            val b = Duration(e.duration.numerator, (e.duration.denominator * 2.0 / 3.0).toInt)
            last = last.drop(1)
            if (e.degree != null) m = m :+ Note(if (e.octave > 0) chord.getScale.getDegreePitch(e.degree) else if (e.octave < 0) chord.getScale.getDegreePitch(e.degree).-.- else chord.getScale.getDegreePitch(e.degree)-, b) else m = m :+ Rest(b) //octave
            j += 1
          }
          Tuplet(3, m)
        } else {
          d += p.duration.getValue()
          c += p.duration.getValue()
          last = last.drop(1)
          if (p.degree != null && chord != null) Note(if (p.octave > 0) chord.getScale.getDegreePitch(p.degree) else if (p.octave < 0 /* && chord.getScale.getDegreePitch(p.degree).-.-.toPitchNumber >*/ ) chord.getScale.getDegreePitch(p.degree).-.- else chord.getScale.getDegreePitch(p.degree)-, p.duration) else Rest(p.duration) //octave
        }
      } else { //no chord ?
        //
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
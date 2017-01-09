package de.htwg.scalamusic

import scala.language.postfixOps

class BassGenerator(val score: Score) {

  var last: Seq[Part] = Seq()

  def generate(): Score = {
    val bassline = generate(score.style, extractChords().music)
    score.copy(music = Seq(Staff(score.music(0).music :+ new Voice(bassline, "electric bass (finger)"))))
  }

  def generate(style: Style, segments: Seq[Repeat]): Seq[Repeat] = {
    if(style == null) {println("No style selected!"); return Seq()}
    val a = Math.random()
    val b = Math.random()
    for (i <- 0 until segments.size) yield { // iterate over measures/ repeats
      if (segments(i).isInstanceOf[Measure]) {
        val bar = segments(i).asInstanceOf[Measure]
        val d = bar.elements.foldLeft(Duration(0, 1))((n, m) => n.sum(m.duration)).getValue()
        if(bar.partial != null) {
          //style.partial(bar.partial)
          bar.copy(clef = Clef.bass, elements = Seq(Rest(bar.partial)), clefChange = segments(0) == bar)
        } else {
          val music: Seq[MusicElement] = applyPattern(bar.elements /*.asInstanceOf[Seq[Chord]]*/ , bar.timeSignature, getPattern(style, a, b), d)
          bar.copy(clef = Clef.bass, elements = music, clefChange = segments(0) == bar)
        }
      } else {
        val rep = segments(i).asInstanceOf[Repeat]
        new Repeat(generate(style, rep.music), rep.alternatives.map { x => if(last.size == 0) generate(style, rep.music ++ x).asInstanceOf[Seq[Measure]].drop(rep.music.size) else generate(style, x).asInstanceOf[Seq[Measure]] })
      }
    }
  }

  def getPattern(style: Style, partA: Double, partB: Double): Seq[Part] = {
    if (last.size > 0) {
      last
    } else {
//      if (count % 4 != 0) {
//        if (count % 2 != 0) {
//          style.patternA((partA * (style.patternA.size - 1)).round.toInt)
//        } else {
//          style.patternA((partB * (style.patternA.size - 1)).round.toInt)
//        }
//      } else {
//        style.patternB((Math.random() * (style.patternB.size - 1)).round.toInt)
//      }
      style.groove((partA * (style.pattern.size - 1)).round.toInt, (partB * (style.pattern.size - 1)).round.toInt, (Math.random() * (style.lick.size - 1)).round.toInt)
    }
  }

  def applyPattern(chords: Seq[MusicElement], time: TimeSignature, pattern: Seq[Part], measureLength: Double): Seq[MusicElement] = {
    val duration = pattern.foldLeft(0.0)((n, m) => n + m.duration.getValue())
    var d = 0.0
    var n = 0
    var c = 0.0
    last = pattern
    var triplets = false
    for (i <- 0 until 30/*???*/; if (d < measureLength && ((pattern(i % pattern.size).duration.denominator % 3 == 0 && !triplets) || pattern(i % pattern.size).duration.denominator % 3 != 0))) yield { ////
      val p = pattern(i % pattern.size)
      c = BigDecimal(c).setScale(7, BigDecimal.RoundingMode.HALF_UP).toDouble // 
      if (c >= chords(n).duration.getValue()) {
        c = c - chords(n).duration.getValue()
        n += 1
      }
      val chord: Chord = if (chords(n).isInstanceOf[Chord]) chords(n).asInstanceOf[Chord] else null
      if (chord != null) {
        if (p.duration.denominator % 3 == 0 && !triplets) { // triplets
          triplets = true
          var j = i
          var m: Seq[MusicElement] = Seq()
          while (pattern(j % pattern.size).duration.denominator % 3 == 0) { // group triplets
            val e = pattern(j % pattern.size)
            d += e.duration.getValue()
            c += e.duration.getValue()
            if (c > chords(n).duration.getValue()) {
              c = c - chords(n).duration.getValue()
              n += 1
            }
            val chord: Chord = chords(n).asInstanceOf[Chord]
            val b = Duration(e.duration.numerator, (e.duration.denominator * 2.0 / 3.0).toInt)
            last = last.drop(1)
            if (e.degree != null) m = m :+ Note(if(e.octave > 0) chord.getScale.getDegreePitch(e.degree) else if(e.octave < 0) chord.getScale.getDegreePitch(e.degree).-.- else chord.getScale.getDegreePitch(e.degree)-, b) else m = m :+ Rest(b)//octave
            j += 1
          }
          Tuplet(3, m)
        } else {
          triplets = false
          d += p.duration.getValue()
          c += p.duration.getValue()
          last = last.drop(1)
          if (p.degree != null && chord != null) Note(if(p.octave > 0) chord.getScale.getDegreePitch(p.degree) else if(p.octave < 0) chord.getScale.getDegreePitch(p.degree).-.- else chord.getScale.getDegreePitch(p.degree)-, p.duration) else Rest(p.duration)//octave
        }
      } else {//no chord ?
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
package de.htwg.scalamusic

import scala.language.postfixOps

class BassGenerator(val score: Score) {

  def generate(style: Style.Value): Score = {
    val chords = extractChords().music
    var t = chords(0).timeSignature
    var k = chords(0).key

    val bassline = for (x1 <- 0 until chords.size) yield { // iterate over measures
      val bar = chords(x1)
      val x2 = x1 + 1
      if (bar.keyChange) k = bar.key
      if (bar.timeChange) t = bar.timeSignature

      val music: Seq[MusicElement] = for (y1 <- 0 until bar.music.size; y2 <- 1 to bar.music.size) yield { // iterate over chords
        val chord = bar.music(y1).asInstanceOf[Chord]
//        val chord = if(bar.music(y1).isInstanceOf[Chord]) bar.music(y1).asInstanceOf[Chord] else bar.music(y1).asInstanceOf[Rest]
        val follow = (if (y2 < bar.music.size) bar.music(y2) else if (x2 < chords.size) chords(x2).music(0) else null).asInstanceOf[Chord]
        //        generatePattern(style, chord, follow, k, t)
        val pattern = generatePattern(style, chord, follow, k, t)

        Pattern(for (i <- Seq.range(0, 10 /**/ ); if (pattern.duration.mul(Beat(i, 1)).getValue < chord.duration.getValue)) yield { // evtl nicht mehr nötig
          if (pattern.duration.getValue > chord.duration.getValue) Pattern(pattern.elements.take(pattern.elements.size / 2)) else pattern
        })
      }
      Measure(timeSignature = t, key = k, clef = Clef.bass, timeChange = bar.timeChange, clefChange = (bar == chords(0)), keyChange = bar.keyChange, music = music)
    }
    score.copy(music = Seq(Staff(score.music(0).music :+ new Voice(bassline, "electric bass (finger)"))))
  }

  def generatePattern(style: Style.Value, chord: Chord, follow: Chord, key: Mode, time: TimeSignature): Pattern = {

    val chordalNotes = chord.music
    val passingNotes = chord.music // needs following chord

    style match {
      case Style.jazz =>
        val note = new Note(chord.root-, new Beat(1, 4))
        Pattern(Seq(note))
      case Style.rock =>
        val note = new Note(chord.root-, new Beat(1, 8))
        Pattern(Seq(note))
      case Style.funk =>
        val note1 = new Note(chord.root-, new Beat(1, 4))
        val note2 = new Note(chord.root, new Beat(1, 4))
        Pattern(Seq(note1, note2))
        
      case Style.beat =>
        val note1 = new Note(chord.root-, new Beat(1, time.denominator * 2))
        val note2 = new Note(Interval.getPitch(chord.root, IntervalQuality.Fifth, key.getSpelling)-, new Beat(1, time.denominator * 2))
        Pattern(Seq(note1, note2))
      case Style.blues =>
        val note1 = new Note(chord.root-, new Beat(1, time.denominator * 2))
        val rest = new Rest(new Beat(1, time.denominator * 2))
        val note2 = new Note(Interval.getPitch(chord.root, IntervalQuality.Fifth, key.getSpelling)-, new Beat(1, time.denominator * 2))
        Pattern(Seq(Tuplet(3, Seq(note1, rest, note2))))
      case Style.blues2 =>
        val note1 = new Note(chord.root-, new Beat(1, time.denominator * 2))
        val rest = new Rest(new Beat(1, time.denominator * 2))
        val note2 = new Note(Interval.getPitch(chord.root, IntervalQuality.MajorThird, key.getSpelling)-, new Beat(1, time.denominator * 2))
        val note3 = new Note(Interval.getPitch(chord.root, IntervalQuality.Fifth, key.getSpelling)-, new Beat(1, time.denominator * 2))
        val note4 = new Note(Interval.getPitch(chord.root, IntervalQuality.MajorSixth, key.getSpelling)-, new Beat(1, time.denominator * 2))
        val note5 = new Note(Interval.getPitch(chord.root, IntervalQuality.Octave, key.getSpelling)-, new Beat(1, time.denominator * 2))
        Pattern(Seq(Tuplet(3, Seq(note1, rest, note1)), Tuplet(3, Seq(note2, rest, note2)), Tuplet(3, Seq(note3, rest, note3)), Tuplet(3, Seq(note4, rest, note5))))
      //        Pattern(fill(chord, time, Pattern(Seq(Tuplet(3, Seq(note1, rest, note1)), Tuplet(3, Seq(note2, rest, note2)), Tuplet(3, Seq(note3, rest, note3)), Tuplet(3, Seq(note4, rest, note5))))))
      case Style.follow =>
        if (follow != null) {
          val note1 = new Note(chord.root-, new Beat(3, chord.duration.denominator * 4))
          val note2 = new Note(key.stepDown(follow.root)-, new Beat(1, chord.duration.denominator * 4))
          Pattern(Seq(note1, note2))
        } else {
          val note = new Note(chord.root-, new Beat(1, chord.duration.denominator))
          Pattern(Seq(note))
        }
      case Style.random =>
        val note = Note(chord.root-, new Beat(1, time.denominator))
        var p = Seq(note)
        var d = note.duration.getValue()
        while (d < chord.duration.getValue()) {
          val diff = chord.duration.getValue() - d
          val r = (Math.random() * (chordalNotes.size - 0.5)).floor.toInt
          val n = if (Math.random() > 0.5 && diff > 1.0 / time.denominator) Note(chordalNotes(r)-, new Beat(1, time.denominator)) else Note(chordalNotes(r)-, new Beat(1, time.denominator * 2))
          p = p :+ n
          d += n.duration.getValue()
        }
        Pattern(p)
      case Style.randomTriplets =>
        val note = Note(chord.root-, new Beat(1, time.denominator))
        var p: Seq[MusicElement] = Seq(note)
        var d = note.duration.getValue()
        while (d < chord.duration.getValue()) {
          //          val diff = chord.duration.getValue() - d

          val r1 = (Math.random() * (chordalNotes.size - 0.5)).floor.toInt
          val r2 = (Math.random() * (chordalNotes.size - 0.5)).floor.toInt
          val r3 = (Math.random() * (chordalNotes.size - 0.5)).floor.toInt
          val rest = Rest(new Beat(1, time.denominator * 2))
          val note1 = Note(chordalNotes(r1)-, new Beat(1, time.denominator * 2))
          val note2 = Note(chordalNotes(r2)-, new Beat(1, time.denominator * 2))
          val note3 = Note(chordalNotes(r3)-, new Beat(1, time.denominator * 2))
          val t = Tuplet(3, Seq(if (Math.random() > 0.1) note1 else rest, if (Math.random() > 0.6) note2 else rest, if (Math.random() > 0.2) note3 else rest))
          p = p :+ t
          d += t.duration.getValue()
        }
        Pattern(p)
      case _ =>
        val note = new Note(chord.root-, new Beat(1, time.denominator))
        Pattern(Seq(note))
    }
  }

  //  def fill(chord: Chord, time: TimeSignature, pattern: Pattern): Seq[MusicElement] = {
  //    var d = 0.0
  //    for (i <- 0 until pattern.elements.size; if (d < chord.duration.getValue())) yield {
  ////      val diff = chord.duration.getValue() - d
  //      d += pattern.elements(i).duration.getValue()
  //      pattern.elements(i)
  //    }
  //  }

  def extractChords(): ChordProgression = {
    score.music.foreach { x => x.music.foreach { m => if (m.isInstanceOf[ChordProgression]) { return m.asInstanceOf[ChordProgression] } } }
    ChordProgression()
  }

}

object Style extends Enumeration {
  type Style = Value
  val jazz, rock, funk, pop, blues, blues2, random, randomTriplets, beat, follow = Value // look for proper styles

  def apply(s: String): Style = withName(s)
}
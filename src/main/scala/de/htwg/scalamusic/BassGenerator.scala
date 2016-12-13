package de.htwg.scalamusic

import scala.language.postfixOps

class BassGenerator(val score: Score) {

  def generate(): Score = {
    val style = score.style
    val chords = extractChords().measures

    var t = chords(0).timeSignature
    var k = chords(0).key

    val bassline = for (x1 <- 0 until chords.size) yield { // iterate over measures
      val bar = chords(x1)
      val x2 = x1 + 1
      if (bar.keyChange) k = bar.key
      if (bar.timeChange) t = bar.timeSignature
      val music: Seq[MusicElement] = (for (y1 <- 0 until bar.music.size; y2 <- 1 to bar.music.size) yield { // iterate over chords
        val follow = (if (y2 < bar.music.size) bar.music(y2) else if (x2 < chords.size) chords(x2).music(0) else null).asInstanceOf[Chord]
        val chord = (if (bar.music(y1).isInstanceOf[Chord]) bar.music(y1) else Chord(root = k.getDegreePitch(ScaleDegree.V), duration = bar.music(y1).duration)).asInstanceOf[Chord]
        fill(chord, t, generatePattern(style, chord, follow, k, t))
      }).flatten
      Measure(timeSignature = t, key = k, clef = Clef.bass, timeChange = bar.timeChange, clefChange = (bar == chords(0)), keyChange = bar.keyChange, music = music)
    }
    score.copy(music = Seq(Staff(score.music(0).music :+ new Voice(bassline, "electric bass (finger)"))))
  }

  def generatePattern(style: Style.Value, chord: Chord, follow: Chord, key: Mode, time: TimeSignature): Seq[MusicElement] = {

    val chordalNotes = chord.music
    val passingNotes = chord.music // needs following chord

    style match {
      case Style.jazz =>
        val note = new Note(chord.root-, new Beat(1, 4))
        Seq(note)
      case Style.rock =>
        val note = new Note(chord.root-, new Beat(1, 8))
        Seq(note)
      case Style.funk =>
        val note1 = new Note(chord.root-, new Beat(1, 4))
        val note2 = new Note(chord.root, new Beat(1, 4))
        Seq(note1, note2)

      case Style.beat =>
        val note1 = new Note(chord.root-, new Beat(1, time.denominator * 2))
        val note2 = new Note(Interval.getPitch(chord.root, IntervalQuality.Fifth, key.getSpelling)-, new Beat(1, time.denominator * 2))
        Seq(note1, note2)
      case Style.blues =>
        val note1 = new Note(chord.root-, new Beat(1, time.denominator * 2))
        val rest = new Rest(new Beat(1, time.denominator * 2))
        val note2 = new Note(Interval.getPitch(chord.root, IntervalQuality.Fifth, key.getSpelling)-, new Beat(1, time.denominator * 2))
        Seq(Tuplet(3, Seq(note1, rest, note2)))
      case Style.blues2 =>
        val note1 = new Note(chord.root-, new Beat(1, time.denominator * 2))
        val rest = new Rest(new Beat(1, time.denominator * 2))
        val note2 = new Note(Interval.getPitch(chord.root, IntervalQuality.MajorThird, key.getSpelling)-, new Beat(1, time.denominator * 2))
        val note3 = new Note(Interval.getPitch(chord.root, IntervalQuality.Fifth, key.getSpelling)-, new Beat(1, time.denominator * 2))
        val note4 = new Note(Interval.getPitch(chord.root, IntervalQuality.MajorSixth, key.getSpelling)-, new Beat(1, time.denominator * 2))
        val note5 = new Note(Interval.getPitch(chord.root, IntervalQuality.Octave, key.getSpelling)-, new Beat(1, time.denominator * 2))
        Seq(Tuplet(3, Seq(note1, rest, note1)), Tuplet(3, Seq(note2, rest, note2)), Tuplet(3, Seq(note3, rest, note3)), Tuplet(3, Seq(note4, rest, note5)))
      case Style.follow =>
        if (follow != null) {
          val note1 = new Note(chord.root-, new Beat(3, chord.duration.denominator * 4))
          val note2 = new Note(key.stepDown(follow.root)-, new Beat(1, chord.duration.denominator * 4))
          Seq(note1, note2)
        } else {
          val note = new Note(chord.root-, new Beat(1, chord.duration.denominator))
          Seq(note)
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
        p
      case Style.randomTriplets =>
        val note = Note(chord.root-, new Beat(1, time.denominator))
        var p: Seq[MusicElement] = Seq(note)
        var d = note.duration.getValue()
        while (d < chord.duration.getValue()) {
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
        p
      case _ =>
        val note = new Note(chord.root-, new Beat(1, time.denominator))
        Seq(note)
    }
  }

  def fill(chord: Chord, time: TimeSignature, pattern: Seq[MusicElement]): Seq[MusicElement] = {
    val duration: Beat = pattern.foldLeft(Beat(0, 1))((n, m) => n.sum(m.duration))
    var d = 0.0
    for (i <- 0 until (chord.duration.getValue / duration.getValue * 2).toInt; if (d < chord.duration.getValue())) yield {
      d += pattern(i % pattern.size).duration.getValue()
      pattern(i % pattern.size)
    }
  }

  def extractChords(): ChordProgression = {
    score.music.foreach { x => x.music.foreach { m => if (m.isInstanceOf[ChordProgression]) { return m.asInstanceOf[ChordProgression] } } }
    ChordProgression()
  }

}
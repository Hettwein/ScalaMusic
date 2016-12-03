package de.htwg.scalamusic

import scala.language.postfixOps

class BassGenerator(val score: Score) {

  def generate(style: String): String = {
    val chords = extractChords()
    var bassline: List[Measure] = List()
    var t = chords.music(0).timeSignature
    var k = chords.music(0).key

    //    val followMap = (chords.music.flatMap { x => x.music }, chords.music.tail.flatMap { x => x.music }).zipped.toMap//.foreach((x, y) => println(x.asLy + " " + y.asLy))

    //    chords.music.foreach { bar => // iterate over measures
    for (x <- 0 until chords.music.size) { // iterate over measures
      val bar = chords.music(x)
      if (bar.keyChange) k = bar.key
      if (bar.timeChange) t = bar.timeSignature
      var measure = Measure(timeSignature = t, key = k, clef = Clef.bass, timeChange = bar.timeChange, clefChange = (bar == chords.music(0)), keyChange = bar.keyChange)
      var music: List[MusicElement] = List()

      //      bar.music.foreach { x => // iterate over chords
      for (y <- 0 until bar.music.size) { // iterate over chords
        val chord = bar.music(y).asInstanceOf[Chord]
        val follow = (if (y + 1 < bar.music.size) bar.music(y + 1) else if (x + 1 < chords.music.size) chords.music(x + 1).music(0) else null).asInstanceOf[Chord]
        //        val follow = followMap.get(chord)
        //        println(follow.get.asLy)
        val pattern = generatePattern(style, chord, follow, k, t)

        val d = pattern.duration
        var i = 0
        while (d.mul(Beat(i, 1)).getValue < chord.duration.getValue) {
          music = music :+ (if (pattern.duration.getValue > chord.duration.getValue) Pattern(pattern.elements.take(pattern.elements.size / 2)) else pattern)
          i += 1
        }

      }
      bassline = bassline :+ measure.copy(music = music)
    }
    val newScore = score.copy(music = Seq(Staff(score.music(0).music :+ new Voice(bassline, "electric bass (pick)"))))
    newScore.asLy + "\n"
  }

  def generatePattern(style: String, chord: Chord, follow: Chord, key: Mode, time: TimeSignature): Pattern = {

    println(if (follow != null) follow.asLy)
    val chordalNotes = chord.music
    val passingNotes = chord.music // needs following chord

    style match {
      case "8Beat" =>
        val note1 = new Note(chord.root-, new Beat(1, time.denominator * 2));
        val note2 = new Note(Interval.getPitch(chord.root, IntervalQuality.Fifth, key.getSpelling)-, new Beat(1, time.denominator * 2));
        Pattern(Seq(note1, note2))
      case "Blues" =>
        val note1 = new Note(chord.root-, new Beat(1, time.denominator * 2));
        val rest = new Rest(new Beat(1, time.denominator * 2));
        val note2 = new Note(Interval.getPitch(chord.root, IntervalQuality.Fifth, key.getSpelling)-, new Beat(1, time.denominator * 2));
        Pattern(Seq(Tuplet(3, Seq(note1, rest, note2))))
      case "Blues2" =>
        val note1 = new Note(chord.root-, new Beat(1, time.denominator * 2));
        val rest = new Rest(new Beat(1, time.denominator * 2));
        val note2 = new Note(Interval.getPitch(chord.root, IntervalQuality.MajorThird, key.getSpelling)-, new Beat(1, time.denominator * 2));
        val note3 = new Note(Interval.getPitch(chord.root, IntervalQuality.Fifth, key.getSpelling)-, new Beat(1, time.denominator * 2));
        val note4 = new Note(Interval.getPitch(chord.root, IntervalQuality.MajorSixth, key.getSpelling)-, new Beat(1, time.denominator * 2));
        val note5 = new Note(Interval.getPitch(chord.root, IntervalQuality.Octave, key.getSpelling)-, new Beat(1, time.denominator * 2));
        Pattern(Seq(Tuplet(3, Seq(note1, rest, note1)), Tuplet(3, Seq(note2, rest, note2)), Tuplet(3, Seq(note3, rest, note3)), Tuplet(3, Seq(note4, rest, note5))))
      case "follow" =>
        if (follow != null) {
          val note1 = new Note(chord.root-, new Beat(3, chord.duration.denominator * 4))
          val note2 = new Note(key.stepDown(follow.root)-, new Beat(1, chord.duration.denominator * 4))
          Pattern(Seq(note1, note2))
        } else {
          val note = new Note(chord.root-, new Beat(1, chord.duration.denominator));
          Pattern(Seq(note))
        }
      case "random" =>
        val note = Note(chord.root-, new Beat(1, time.denominator))
        var p = Seq(note)
        var d = note.duration.getValue()
        while(d < chord.duration.getValue()) {
          val r = (Math.random() * (chordalNotes.size - 0.4)).floor.toInt
          val n = Note(chordalNotes(r)-, new Beat(1, time.denominator))
          p = p :+ n
          d += n.duration.getValue()
        }
        Pattern(p)
      case _ =>
        val note = new Note(chord.root-, new Beat(1, time.denominator))
        Pattern(Seq(note))
    }
  }

  def extractChords(): ChordProgression = {
    score.music.foreach { x => x.music.foreach { m => if (m.isInstanceOf[ChordProgression]) { return m.asInstanceOf[ChordProgression] } } }
    ChordProgression()
  }

}
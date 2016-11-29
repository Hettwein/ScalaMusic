package de.htwg.scalamusic

class BassGenerator(val score: Score) {

  def generate() {
    val chords = extractChords()
    var bassline: List[Measure] = List()
    chords.music.foreach { bar =>
      val t = bar.timeSignature
      val k = bar.key
      var measure = Measure(timeSignature = t, key = k, clef = Clef.bass, clefChange = true)
      var music: List[MusicElement] = List()
      bar.music.foreach { x =>
        val chord = x.asInstanceOf[Chord]
        val note = new Note(chord.root.copy(octave = chord.root.octave - 1), new Beat(1, t.denominator));
        for (i <- 0 until chord.duration.numerator * t.denominator / chord.duration.denominator)
          music = music:+note
      }
      bassline = bassline:+measure.copy(music = music)
    }
    val newScore = score.copy(music = Seq(Staff(score.music(0).music:+new Voice(bassline, "electric bass (pick)"))))
    println(newScore.asLy)
  }

  def extractChords(): ChordProgression = {
    score.music.foreach { x => x.music.foreach { m => if (m.isInstanceOf[ChordProgression]) { return m.asInstanceOf[ChordProgression] } } }
    return ChordProgression()
  }
}
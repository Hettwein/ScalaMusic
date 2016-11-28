package de.htwg.scalamusic.music

class BassGenerator(val score: Score) {

  def generate() {
    val chords = extractChords()
    var bassline = new Voice()
    //    val time = score.timeSignature
    //    chords.music.foreach { x => val note = new Note(x.root.copy(octave = x.root.octave - 1), new Beat(1, time.denominator)); for(i <- 0 until x.duration.numerator * time.denominator / x.duration.denominator) bassline.add(note) }
    //    score.add(new Staff(clef = Clef.bass, music = Seq(bassline)))
    println(score.asLy)
  }

  def extractChords(): ChordProgression = {
    score.music.foreach { x => x.music.foreach { m => if (m.isInstanceOf[ChordProgression]) { return m.asInstanceOf[ChordProgression] } } }
    return ChordProgression(instrument = "electric bass (pick)")
  }
}
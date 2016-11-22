package de.htwg.scalamusic.music

object Main extends App {

  val chord = new Chord(new Pitch(PitchClass.A, PitchDecorator.Blank, 0), ChordQuality.Major, new Beat(1, 1), 70)
  val chord1 = new Chord(new Pitch(PitchClass.C, PitchDecorator.Sharp, 0), ChordQuality.Minor)
  val note = new Note(new Pitch(PitchClass.C, PitchDecorator.Sharp, 0), new Beat(1, 4), 70)
  val note1 = new Note(new Pitch(PitchClass.F, PitchDecorator.Blank, 0), new Beat(3, 8), 70)

  val chords = new ChordProgression(Seq(chord))
  chords add chord1
  val voice = new Voice(Seq(note))
  voice add note
  voice add note
  voice add note1
  voice add note1

  val staff = new Staff(Clef.bass, new MajorScale(Pitch("a")))
  staff add chords
  staff add voice
  val score = new Score(115, new TimeSignature(3, 4))
  score add staff

  println(score.asLy)
  //  println(score(0)(0).asLy)
}
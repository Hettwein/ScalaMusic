# ScalaMusic

This project is based on [scala-music-dsl](https://github.com/RadicalCadence/scala-music-dsl).

###For trying out:

1. [Scala](http://www.scala-lang.org/), [SBT](http://www.scala-sbt.org/) and [LilyPond](http://www.lilypond.org/index.html) have to be installed correctly
2. clone this repository
3. open a terminal and navigate to the root of this repository
4. start the Scala interpreter by typing `sbt console`
5. now you can type in your commands:

e.g. `generate"style 'rock' chords( |: tempo 105 c:2 em:2 | f:2 g.7:2 | c:2 am:2 | [dm:2 g:2|][dm:2 g:2|] :|)"`

possible commands are:
* `generate`: generates a bassline based on a chord progression and a style and creates a LilyPond file, a PDF and a MIDI file
* `m`:        gibt das Objekt als DSL-String zurück
* `ly`:       gibt das Objekt als LilyPond-String zurück
* `show`:     creates a LilyPond file, a PDF and a MIDI file

the created files should be in the current directory

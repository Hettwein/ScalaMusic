# ScalaMusic

This project is originally based on [scala-music-dsl](https://github.com/RadicalCadence/scala-music-dsl).

###Installation Guide for developers:

1. [Scala](http://www.scala-lang.org/), [SBT](http://www.scala-sbt.org/) and [LilyPond](http://www.lilypond.org/index.html) have to be installed correctly (especially make sure LilyPond is added to the environment variable 'Path')
2. clone this repository
3. open a terminal and navigate to the root of this repository
4. start the Scala interpreter by typing `sbt console`
5. now you can type in your commands:

e.g. `generate"style 'rock' chords( |: tempo 105 c:2 em:2 | f:2 g.7:2 | c:2 am:2 | [dm:2 g:2|][dm:2 g:2|] :|)"`

possible commands are:
* `generate`: generates a bassline based on a chord progression and a style and creates a LilyPond file, a PDF and a MIDI file
* `m`:        returns the music object as full DSL string
* `ly`:       returns the music object as LilyPond string
* `show`:     creates a LilyPond file, a PDF and a MIDI file

the created files should be in a directory called 'lilypond-output' and should be opened automatically

###User Guide:

If you just want to try out the DSL:

1. [Java](https://java.com/de/download/) and [LilyPond](http://www.lilypond.org/index.html) have to be installed correctly (again: don't forget to add it to the 'Path')
2. just download the files contained in the directory 'explore'
3. on Windows you can just run ScalaMusic.bat and get started
4. else: open a terminal and navigate to these files, then enter `java -classpath ScalaMusic-assembly-0.0.1.jar de.htwg.scalamusic.Main`

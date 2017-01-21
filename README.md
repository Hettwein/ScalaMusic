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

If you want to use the DSL:

1. [Java](https://java.com/de/download/) and [LilyPond](http://www.lilypond.org/index.html) have to be installed correctly (again: don't forget to add it to the 'Path')
2. just download the files contained in the directory 'bin'
3. on Windows you can just run ScalaMusic.bat and get started
4. on Unix-based operating systems try ScalaMusic.sh
5. else: open a terminal and navigate to the right directory, then enter `java -classpath ScalaMusic-assembly-0.0.1.jar de.htwg.scalamusic.Main`

You need to create an input file first which contains your DSL string. Name it as you wish, the file extension doesn't matter.
You can use spaces and newlines for better clarity inside the file.



###EBNF:

A complete description of the DSL in the extended Backus–Naur form:

```
Score ::= ["("] [Style] Staff {Staff} [")"]
Style ::= "style" "'" ("rock" | "jazz" | "funk" | "soul" | "slowrock" | "boogie" | "reggae" | "swing" | ... ) "'"
Staff ::= ["("] (Voice | ChordProgression) {Voice | ChordProgression} [")"]
Voice ::= ["("] [Instrument] (measure | repeat) {measure | repeat} [")"]
Instrument ::= "instr" "'" ("acoustic grand" | ... ) "'"
Measure ::= ["|"] [TimeSignature] [Key] [Clef] [Tempo] [Partial] Element {Element} ["|"]
TimeSignature ::= Quantity "/" Duration
Quantity ::= Digit {Digit}
Digit ::= {"0"| "1"| "2"| "3"| "4"| "5"| "6"| "7"| "8"| "9"}
Duration ::= ("1" | "2" | "4" | "8" | "16" | "32") ["."] {"~" ["|"] Element}
Element ::= Note | Rest | Chord | Tuplet
Note ::= Pitch Duration
Pitch ::= PitchClass [PitchDecorator] [Octave]
PitchClass ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "A" | "B" | "C" | "D" | "E" | "F" | "G"
PitchDecorator ::= "isis" | "is" | "eses" | "ses" | "es" | "s"
Octave ::= {"'" | ","}
Rest ::= ("r" | "R") Duration
Chord ::= Pitch ChordQuality ":" Duration
ChordQuality ::= "" | "min" | ".7" | ".6" | ".5" | "dim" | "aug" | "M7" | "m7" | ".sus4" | ".sus2" | ".9" | "M" | "m"
Tuplet ::= "{" Element {Element} "}" Quantity
Key ::= "key" Pitch ["m"]
Clef ::= "clef" ("treble" | "alt" | "tenor" | "bass")
Tempo ::= "tempo" Quantity
Partial ::= "partial" Duration
Repeat ::= "|:" (Measure | Repeat) {Measure | Repeat} {"[" Measure {Measure} "]"} ":|"
ChordProgression ::= "chords" "(" [Instrument] (measure | repeat) {measure | repeat} ")"
```


**Explanation:**

* optional: [ ... ]
* alternative: ... | ...
* grouped: ( ... )
* any number (0-n): { ... }

You can find a complete list of midi instruments [here](http://lilypond.org/doc/v2.18/Documentation/notation/midi-instruments).

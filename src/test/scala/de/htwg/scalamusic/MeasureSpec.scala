package de.htwg.scalamusic

import org.scalatest.WordSpec
import org.scalatest.Matchers
import scala.language.postfixOps
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MeasureSpec extends WordSpec with Matchers {

  "Measures" should {
    "be easy to create" in {
      Measure(TimeSignature(), MinorScale(Pitch()), Clef.treble, 125, Seq(Note(), Note(), Note(), Note()), null, true, true, true, true)
    }
    "have a time signature" in {
      Measure(timeSignature = TimeSignature()).timeSignature should be(TimeSignature(4, 4))
    }
    "have a key" in {
      Measure(key = MajorScale(Pitch())).key should be(MajorScale(Pitch("c")))
    }
    "have a clef" in {
      Measure(clef = Clef.bass).clef should be(Clef.bass)
    }
    "have a tempo" in {
      Measure(tempo = 77).tempo should be(77)
    }
    "have a set of music elements" in {
      Measure(elements = Seq(Note(), Note(), Note(), Note())).elements should be(Seq(Note(), Note(), Note(), Note()))
    }
    "have a string representation for clefs" in {
      Clef.toString(Clef.tenor) should be("clef tenor ")
      Clef("alto") should be(Clef.alto)
    }
    "have a DSL representation" in {
      Measure(elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("c4 c4 c4 c4 |")
      Measure(timeChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("4/4 c4 c4 c4 c4 |")
      Measure(timeChange = true, keyChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("4/4 key c c4 c4 c4 c4 |")
      Measure(timeChange = true, clefChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("4/4 clef treble c4 c4 c4 c4 |")
      Measure(timeChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("4/4 tempo 105 c4 c4 c4 c4 |")
      Measure(timeChange = true, keyChange = true, clefChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("4/4 key c clef treble c4 c4 c4 c4 |")
      Measure(timeChange = true, keyChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("4/4 key c tempo 105 c4 c4 c4 c4 |")
      Measure(timeChange = true, clefChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("4/4 clef treble tempo 105 c4 c4 c4 c4 |")
      Measure(timeChange = true, keyChange = true, clefChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("4/4 key c clef treble tempo 105 c4 c4 c4 c4 |")
      Measure(keyChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("key c c4 c4 c4 c4 |")
      Measure(keyChange = true, clefChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("key c clef treble c4 c4 c4 c4 |")
      Measure(keyChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("key c tempo 105 c4 c4 c4 c4 |")
      Measure(keyChange = true, clefChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("key c clef treble tempo 105 c4 c4 c4 c4 |")
      Measure(clefChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("clef treble c4 c4 c4 c4 |")
      Measure(clefChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("clef treble tempo 105 c4 c4 c4 c4 |")
      Measure(tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asDSL should be("tempo 105 c4 c4 c4 c4 |")
    }
    "have a LilyPond representation" in {
      Measure(elements = Seq(Note(), Note(), Note(), Note())).asLy should be("c4 c4 c4 c4 |")
      Measure(timeChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\time 4/4 c4 c4 c4 c4 |")
      Measure(timeChange = true, keyChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\time 4/4 \\key c \\major c4 c4 c4 c4 |")
      Measure(timeChange = true, clefChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\time 4/4 \\clef treble c4 c4 c4 c4 |")
      Measure(timeChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\time 4/4 \\tempo 4 = 105 c4 c4 c4 c4 |")
      Measure(timeChange = true, keyChange = true, clefChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\time 4/4 \\key c \\major \\clef treble c4 c4 c4 c4 |")
      Measure(timeChange = true, keyChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\time 4/4 \\key c \\major \\tempo 4 = 105 c4 c4 c4 c4 |")
      Measure(timeChange = true, clefChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\time 4/4 \\clef treble \\tempo 4 = 105 c4 c4 c4 c4 |")
      Measure(timeChange = true, keyChange = true, clefChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\time 4/4 \\key c \\major \\clef treble \\tempo 4 = 105 c4 c4 c4 c4 |")
      Measure(keyChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\key c \\major c4 c4 c4 c4 |")
      Measure(keyChange = true, clefChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\key c \\major \\clef treble c4 c4 c4 c4 |")
      Measure(keyChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\key c \\major \\tempo 4 = 105 c4 c4 c4 c4 |")
      Measure(keyChange = true, clefChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\key c \\major \\clef treble \\tempo 4 = 105 c4 c4 c4 c4 |")
      Measure(clefChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\clef treble c4 c4 c4 c4 |")
      Measure(clefChange = true, tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\clef treble \\tempo 4 = 105 c4 c4 c4 c4 |")
      Measure(tempoChange = true, elements = Seq(Note(), Note(), Note(), Note())).asLy should be("\\tempo 4 = 105 c4 c4 c4 c4 |")
    }
  }

}
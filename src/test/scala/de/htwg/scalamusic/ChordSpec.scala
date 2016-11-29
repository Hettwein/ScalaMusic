package de.htwg.scalamusic

import org.scalatest.WordSpec
import org.scalatest.Matchers
import scala.language.postfixOps
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ChordSpec extends WordSpec with Matchers {

  "Chords" should {
    "have a root" in {
      Chord(Pitch("e")).root should be(Pitch("e"))
    }
    "have a quality" in {
      Chord(quality = ChordQuality.Minor).quality should be(ChordQuality.Minor)
    }
    "have a duration" in {
      Chord(duration = Beat(1, 2)).duration should be(Beat(1, 2))
    }
    "have a tied annotation" in {
      Chord(tied = true).tied should be(true)
    }
    "have a volume" in {
      Chord(velocity = 84).velocity should be(84)
    }
    "have a set of pitches" in {
      Chord().music should be(Seq(Pitch("c"), Pitch("e"), Pitch("g")))
    }
    "have a string representation of their quality" in {
      ChordQuality("m") should be(ChordQuality.Minor)
      ChordQuality("M") should be(ChordQuality.Major)
    }
    "have a DSL representation" in {
    	Chord().asDSL should be("< c e g >1")
    	Chord(Pitch("es")).asDSL should be("< ees g bes >1")
    	Chord(Pitch("f"), ChordQuality.Minor, Beat(1, 2)).asDSL should be("< f aes c' >2")
    	Chord(Pitch("a"), ChordQuality.Minor, Beat(1, 8), true).asDSL should be("< a c' e' >8~")
    	Chord(Pitch("d"), ChordQuality.Minor, Beat(3, 4)).asDSL should be("< d f a >2.")
    }
    "have a LilyPond representation" in {
      Chord().asLy should be("< c e g >1")
      Chord(Pitch("es")).asLy should be("< ees g bes >1")
      Chord(Pitch("f"), ChordQuality.Minor, Beat(1, 2)).asLy should be("< f aes c' >2")
      Chord(Pitch("a"), ChordQuality.Minor, Beat(1, 8), true).asLy should be("< a c' e' >8~")
      Chord(Pitch("d"), ChordQuality.Minor, Beat(3, 4)).asLy should be("< d f a >2.")
    }
  }

}
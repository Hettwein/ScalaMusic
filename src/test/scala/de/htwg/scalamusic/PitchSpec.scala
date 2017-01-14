package de.htwg.scalamusic

import org.scalatest.WordSpec
import org.scalatest.Matchers
import scala.language.postfixOps
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PitchSpec extends WordSpec with Matchers {
  import parser._

  "Pitches" should {
    "be easy to create from the REPL" in {
      //      val p1 = Pitch(60, KeySignatureSpelling.Sharps)
      val p2 = Pitch(pitchClass = PitchClass.A)
      val p3 = Pitch(pitchClass = PitchClass.F, decorator = PitchDecorator.Sharp)
      val p4 = Pitch(pitchClass = PitchClass.B, decorator = PitchDecorator.Flat, octave = 2)
      val p5 = Pitch("e")
      val p6 = Pitch("cis")
      val p7 = Pitch("cis'")
    }
    "have a PitchClass" in {
      Pitch(pitchClass = PitchClass.C).pitchClass should be(PitchClass.C)
      Pitch(pitchClass = PitchClass.D).pitchClass should be(PitchClass.D)
      Pitch(pitchClass = PitchClass.E).pitchClass should be(PitchClass.E)
      Pitch(pitchClass = PitchClass.F).pitchClass should be(PitchClass.F)
      Pitch(pitchClass = PitchClass.G).pitchClass should be(PitchClass.G)
      Pitch(pitchClass = PitchClass.A).pitchClass should be(PitchClass.A)
      Pitch(pitchClass = PitchClass.B).pitchClass should be(PitchClass.B)
      Pitch().pitchClass should be(PitchClass.C)
    }
    "have a PitchDecorator" in {
      Pitch(decorator = PitchDecorator.Sharp).decorator should be(PitchDecorator.Sharp)
      Pitch(decorator = PitchDecorator.Flat).decorator should be(PitchDecorator.Flat)
      Pitch(decorator = PitchDecorator.DoubleSharp).decorator should be(PitchDecorator.DoubleSharp)
      Pitch(decorator = PitchDecorator.DoubleFlat).decorator should be(PitchDecorator.DoubleFlat)
      Pitch(decorator = PitchDecorator.Natural).decorator should be(PitchDecorator.Natural)
      Pitch(decorator = PitchDecorator.Blank).decorator should be(PitchDecorator.Blank)
      Pitch().decorator should be(PitchDecorator.Blank)
    }
    "have an octave" in {
      Pitch(octave = 1).octave should be(1)
      Pitch().octave should be(0)
    }
    "have a string representation" in {
      Pitch("d") should be(Pitch(PitchClass.D))
      Pitch("dis") should be(Pitch(PitchClass.D, PitchDecorator.Sharp))
      Pitch("d'") should be(Pitch(PitchClass.D, octave = 1))
      Pitch("dis,") should be(Pitch(PitchClass.D, PitchDecorator.Sharp, -1))
      Pitch("es,,").toString() should be("ees,,")
      Pitch("gisis''").toString() should be("gisis''")
    }
    "be convertible to a midi number" in {
      Pitch(PitchClass.B, PitchDecorator.Flat, 2).toPitchNumber should be(34)
      Pitch().toPitchNumber should be(0)
      //      Pitch(7, KeySignatureSpelling.Sharps) should be(Pitch(PitchClass.G))
      //      Pitch(-5, KeySignatureSpelling.Sharps) should be(Pitch(PitchClass.G, octave = -1))
    }
    "be able to use default values" in {
      val p = Pitch(PitchClass.C, PitchDecorator.Blank, 0)
      Pitch() should be(p)
      Pitch(PitchClass.C) should be(p)
      Pitch(PitchClass.C, PitchDecorator.Blank) should be(p)
      Pitch(decorator = PitchDecorator.Blank) should be(p)
      Pitch(octave = 0) should be(p)
    }
  }

}
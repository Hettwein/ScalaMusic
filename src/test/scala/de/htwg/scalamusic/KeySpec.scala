package de.htwg.scalamusic

import org.scalatest.WordSpec
import org.scalatest.Matchers
import scala.language.postfixOps
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class KeySpec extends WordSpec with Matchers {
  "Keys" should {
    "be one of several different scales with a root pitch" in {
      val k1 = MajorScale(Pitch())
      val k2 = MinorScale(Pitch())
    }
    //    "have indicated which kind of signs they have" in {
    //      MajorScale(Pitch("f")).getSpelling should be(KeySignatureSpelling.Flats)
    //      MajorScale(Pitch("g")).getSpelling should be(KeySignatureSpelling.Sharps)
    //      MinorScale(Pitch("g")).getSpelling should be(KeySignatureSpelling.Flats)
    //      MinorScale(Pitch("b")).getSpelling should be(KeySignatureSpelling.Sharps)
    //    }
    "have degree pitches which can be requested individually" in {
      MajorScale(Pitch("e")).getDegreePitch(ScaleDegree.I) should be(Pitch("e"))
      MajorScale(Pitch("e")).getDegreePitch(ScaleDegree.II) should be(Pitch("fis"))
      MajorScale(Pitch("e")).getDegreePitch(ScaleDegree.III) should be(Pitch("gis"))
      MajorScale(Pitch("e")).getDegreePitch(ScaleDegree.IV) should be(Pitch("a"))
      MajorScale(Pitch("e")).getDegreePitch(ScaleDegree.V) should be(Pitch("b"))
      MajorScale(Pitch("e")).getDegreePitch(ScaleDegree.VI) should be(Pitch("cis'"))
      MajorScale(Pitch("e")).getDegreePitch(ScaleDegree.VII) should be(Pitch("dis'"))
      MinorScale(Pitch("c")).getDegreePitch(ScaleDegree.I) should be(Pitch("c"))
      MinorScale(Pitch("c")).getDegreePitch(ScaleDegree.II) should be(Pitch("d"))
      MinorScale(Pitch("c")).getDegreePitch(ScaleDegree.III) should be(Pitch("es"))
      MinorScale(Pitch("c")).getDegreePitch(ScaleDegree.IV) should be(Pitch("f"))
      MinorScale(Pitch("c")).getDegreePitch(ScaleDegree.V) should be(Pitch("g"))
      MinorScale(Pitch("c")).getDegreePitch(ScaleDegree.VI) should be(Pitch("as"))
      MinorScale(Pitch("c")).getDegreePitch(ScaleDegree.VII) should be(Pitch("bes"))
    }
    //    "have a DSL representation" in {
    //      MajorScale(Pitch("fis")).asDSL should be("key fis ")
    //      MinorScale(Pitch("es")).asDSL should be("key eesm ")
    //    }
    //    "have a LilyPond representation" in {
    //      MajorScale(Pitch("fis")).asLy should be("\\key fis \\major ")
    //      MinorScale(Pitch("es")).asLy should be("\\key ees \\minor ")
    //    }
  }
}
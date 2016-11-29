package de.htwg.scalamusic

import org.scalatest.WordSpec
import org.scalatest.Matchers
import scala.language.postfixOps
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NoteSpec extends WordSpec with Matchers {

  "Notes" should {
    "have a pitch" in {
      Note(Pitch("e")).pitch should be(Pitch("e"))
    }
    "have a duration" in {
    	Note(duration = Beat(1, 1)).duration should be(Beat(1, 1))
    }
    "have a tied annotation" in {
    	Note(tied = true).tied should be(true)
    }
    "have a volume" in {
    	Note(velocity = 84).velocity should be(84)
    }
    "have a DSL representation" in {
    	Note().asDSL should be("c4")
    	Note(Pitch("es")).asDSL should be("ees4")
    	Note(Pitch("f"), Beat(1, 2)).asDSL should be("f2")
    	Note(Pitch("a"), Beat(1, 8), true).asDSL should be("a8~")
    	Note(Pitch("d"), Beat(3, 4)).asDSL should be("d2.")
    }
    "have a LilyPond representation" in {
      Note().asLy should be("c4")
      Note(Pitch("es")).asLy should be("ees4")
      Note(Pitch("f"), Beat(1, 2)).asLy should be("f2")
      Note(Pitch("a"), Beat(1, 8), true).asLy should be("a8~")
      Note(Pitch("d"), Beat(3, 4)).asLy should be("d2.")
    }
  }

}
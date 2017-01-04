package de.htwg.scalamusic

import org.scalatest.WordSpec
import org.scalatest.Matchers
import scala.language.postfixOps
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RestSpec extends WordSpec with Matchers {
  "Rests" should {
    "have a duration" in {
      Note(duration = Duration(1, 1)).duration should be(Duration(1, 1))
    }
    "have a DSL representation" in {
      Rest(Duration(1, 2)).asDSL should be("r2")
    }
    "have a LilyPond representation" in {
      Rest(Duration(1, 2)).asLy should be("r2")
    }
  }
}
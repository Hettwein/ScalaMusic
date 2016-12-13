package de.htwg.scalamusic

trait MusicConversion {
  //  /** Format this Music object as a LilyPond string. */
  //  def asLy: String
  //
  //  /** Format this Music object as a DSL string. */
  //  def asDSL: String
  def asLy: String = this.toString().toLowerCase() //name + duration + attribute + " "
  def asDSL: String = this.toString().toLowerCase()
}

trait MusicSegment extends MusicConversion
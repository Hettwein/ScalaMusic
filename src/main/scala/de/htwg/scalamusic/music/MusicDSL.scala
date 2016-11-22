package de.htwg.scalamusic.music

trait MusicDSL {
  //  /** Format this Music object as a LilyPond string. */
  //  def asLy: String
  //
  //  /** Format this Music object as a DSL string. */
  //  def asDSL: String
  def asLy: String = this.toString().toLowerCase() //name + duration + attribute + " "
  def asDSL: String = this.toString().toLowerCase()
}
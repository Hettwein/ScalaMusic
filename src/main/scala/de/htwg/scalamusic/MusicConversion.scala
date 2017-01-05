package de.htwg.scalamusic

trait MusicConversion {
  /** Format this Music object as a LilyPond string. */
  def asLy: String

  /** Format this Music object as a DSL string. */
  def asDSL: String
}

trait MusicElement extends MusicConversion with Traversable[MusicElement] {
  val duration: Duration
  def foreach[U](f: MusicElement => U) = f(this)
}
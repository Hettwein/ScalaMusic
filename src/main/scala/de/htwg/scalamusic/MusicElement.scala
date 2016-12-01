package de.htwg.scalamusic

trait MusicElement extends MusicConversion with Traversable[MusicElement] {
  val duration: Beat
  val tied: Boolean = false
  def foreach[U](f: MusicElement => U) = f(this)
}
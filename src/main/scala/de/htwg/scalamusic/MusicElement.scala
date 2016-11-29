package de.htwg.scalamusic

trait MusicElement extends MusicConversion {
  val duration: Beat
  val tied: Boolean
}
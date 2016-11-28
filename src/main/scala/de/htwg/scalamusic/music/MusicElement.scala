package de.htwg.scalamusic.music

trait MusicElement extends MusicConversion {
  val duration: Beat
  val tied: Boolean
}
package de.htwg.scalamusic.music

trait Music extends MusicConversion

trait MusicComposite[M <: Music] extends Music {
  var music: Seq[M]

  def add(child: M) = {
    music = music :+ (child)
  }
  def remove(child: M) = {
    music = music.filterNot { _ == child }
  }
  def apply(index: Int) = music(index)
}

trait MusicSequence[M <: MusicElement] extends MusicComposite[M]

trait MusicElement extends Music {
  val duration: Beat
}
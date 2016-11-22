package de.htwg.scalamusic.music

case class Staff(clef: Clef.Value = Clef.treble, key: Mode = MajorScale(Pitch()), var music: Seq[MusicSequence[_]] = Seq()) extends MusicComposite[MusicSequence[_]] {
  override def asLy: String = s"""|
                              |  <<
                              |    ${music.foldLeft("")((s, m) => s + m.asLy + " ")}
                              |    {\\clef ${clef} ${key.asLy}}
                              |  >>""".stripMargin
  override def asDSL: String = s"""<< ${music.foldLeft("")((s, m) => s + m.asDSL + " ")}>>"""
}

object Clef extends Enumeration {
  type Clef = Value
  val treble, alto, tenor, bass = Value
  def apply(s: String): Clef = withName(s)
}
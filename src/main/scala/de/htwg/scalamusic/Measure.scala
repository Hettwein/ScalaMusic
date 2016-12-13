package de.htwg.scalamusic

case class Measure(
    timeSignature: TimeSignature = TimeSignature(),
    key: Mode = MajorScale(Pitch()),
    clef: Clef.Value = Clef.treble,
    tempo: Int = 105,
    music: Seq[MusicElement] = Seq(),
    keyChange: Boolean = false,
    timeChange: Boolean = false,
    clefChange: Boolean = false,
    tempoChange: Boolean = false,
    partial: Int = 0
) extends MusicSegment {

  override def asLy: String = s"""${if (timeChange) timeSignature.asLy else ""}${if (keyChange) key.asLy else ""}${if (clefChange) "\\" + Clef.toString(clef) else ""}${if (tempoChange) "\\tempo " + timeSignature.denominator + " = " + tempo + " " else ""}${if (partial != 0) "\\partial " + partial + " " else ""}${music.foldLeft("")((s, m) => s + m.asLy + " ")}|"""
  override def asDSL: String = s"""${if (timeChange) timeSignature.asDSL else ""}${if (keyChange) key.asDSL else ""}${if (clefChange) Clef.toString(clef) else ""}${if (tempoChange) "tempo " + tempo + " " else ""}${if (partial != 0) "partial " + partial + " " else ""}${music.foldLeft("")((s, m) => s + m.asDSL + " ")}|"""
}

object Clef extends Enumeration {
  type Clef = Value
  val treble, alto, tenor, bass = Value
  def apply(s: String): Clef = withName(s)
  def toString(value: Value): String = "clef " + value + " "
}
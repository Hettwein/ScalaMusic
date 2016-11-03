package de.htwg.music.elements

import de.htwg.music.MusicElement
import de.htwg.music.SignMode

case class Note(pitch: Int, value: Double = 4, volume: Int = 75, sign: SignMode.Value = SignMode.None, tied: Double = 0) extends MusicElement {

  require(0 <= pitch && pitch <= 128)
  require(0 <= volume && volume <= 100)

  val keynumber = pitch % 12
  val octave = pitch / 12 - 1

  def sharp = copy(pitch = pitch + 1, sign = SignMode.Sharp)
  def flat = copy(pitch = pitch - 1, sign = SignMode.Flat)
  def natural = copy(pitch = if (sign == SignMode.Sharp) (pitch - 1) else if (sign == SignMode.Flat) (pitch + 1) else pitch, sign = SignMode.Natural)
  def dot = copy(value = value * 1.5)
  def tie(note: Note) = copy(tied = note.value)

  def + = copy(pitch = pitch + 12)
  def ++ = copy(pitch = pitch + 24)
  def +++ = copy(pitch = pitch + 36)
  def - = copy(pitch = pitch - 12)
  def -- = copy(pitch = pitch - 24)
  def --- = copy(pitch = pitch - 36)

  val keynumberToString = Map(
    0 -> (if (sign == SignMode.Sharp) "B\u266F" else "C"),
    1 -> (if (sign == SignMode.Sharp) "C\u266F" else if (sign == SignMode.Flat) "D\u266D"),
    2 -> "D",
    3 -> (if (sign == SignMode.Sharp) "D\u266F" else if (sign == SignMode.Flat) "E\u266D"),
    4 -> (if (sign == SignMode.Flat) "F\u266D" else "E"),
    5 -> (if (sign == SignMode.Sharp) "E\u266F" else "F"),
    6 -> (if (sign == SignMode.Sharp) "F\u266F" else if (sign == SignMode.Flat) "G\u266D"),
    7 -> "G",
    8 -> (if (sign == SignMode.Sharp) "G\u266F" else if (sign == SignMode.Flat) "A\u266D"),
    9 -> "A",
    10 -> (if (sign == SignMode.Sharp) "A\u266F" else if (sign == SignMode.Flat) "B\u266D"),
    11 -> (if (sign == SignMode.Flat) "C\u266D" else "B"))

  val octaveToString = Map(
    -1 -> ",,,,,",
    0 -> ",,,,",
    1 -> ",,,",
    2 -> ",,",
    3 -> ",",
    4 -> "",
    5 -> "'",
    6 -> "\"",
    7 -> "\"'",
    8 -> "\"\"",
    9 -> "\"\"'")

  val valueToString = Map(
    1 -> "\u1D15D",
    1.5 -> "\u1D15E\u00B7",
    2 -> "\u1D15E",
    8.0/3.0 -> "\u1D15F\u00B7",
    3 -> "\u1D15E³",
    4 -> "\u1D15F",
    16.0/3.0 -> "\u1D160\u00B7",
    6 -> "\u1D15F³",
    8 -> "\u1D160",
    12 -> "\u1D160³",
    16 -> "\u1D161")

  def play() = {

  }

  override def toString = if (tied == 0) keynumberToString(keynumber) + octaveToString(octave) + valueToString(value) else keynumberToString(keynumber) + octaveToString(octave) + valueToString(value) + "_" + valueToString(tied)
  override def equals(that: Any): Boolean =
    that match {
      case that: Note => (this.pitch == that.pitch) && (this.value == that.value)
      case _ => false
    }
}
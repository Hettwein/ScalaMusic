package de.htwg.music

import de.htwg.music.elements.Note

case class Key(root: Note, mode: ScaleType.Value = ScaleType.Major) {

  val scale: Vector[Note] = {
    var s: Vector[Note] = ScaleType.scaleVector(mode).map(i => root.copy(pitch = root.pitch + i))
    signs.pos.map(i => s.updated(i,  s(i).copy(sign = signs.mode)))
    s
  }

  def signs(): Sign = {
    sign(root)
  }

  val sign = Map(
    C -> Sign(Vector(), SignMode.Sharp),
    a -> Sign(Vector(), SignMode.Sharp),

    G -> Sign(Vector(6), SignMode.Sharp),
    e -> Sign(Vector(1), SignMode.Sharp),
    D -> Sign(Vector(6, 2), SignMode.Sharp),
    b -> Sign(Vector(1, 5), SignMode.Sharp),
    A -> Sign(Vector(6, 2, 5), SignMode.Sharp),
    fis -> Sign(Vector(1, 5, 0), SignMode.Sharp),
    E -> Sign(Vector(6, 2, 5, 1), SignMode.Sharp),
    cis -> Sign(Vector(1, 5, 0, 4), SignMode.Sharp),
    B -> Sign(Vector(6, 2, 5, 1, 4), SignMode.Sharp),
    ais -> Sign(Vector(1, 5, 0, 4, 6), SignMode.Sharp),
    Fis -> Sign(Vector(6, 2, 5, 1, 4, 0), SignMode.Sharp),
    eis -> Sign(Vector(1, 5, 0, 4, 6, 2), SignMode.Sharp),

    F -> Sign(Vector(3), SignMode.Flat),
    d -> Sign(Vector(5), SignMode.Flat),
    Bes -> Sign(Vector(3, 0), SignMode.Flat),
    g -> Sign(Vector(5, 2), SignMode.Flat),
    Es -> Sign(Vector(3, 0, 4), SignMode.Flat),
    c -> Sign(Vector(5, 2, 6), SignMode.Flat),
    As -> Sign(Vector(3, 0, 4, 1), SignMode.Flat),
    f -> Sign(Vector(5, 2, 6, 3), SignMode.Flat),
    Des -> Sign(Vector(3, 0, 4, 1, 5), SignMode.Flat),
    bes -> Sign(Vector(5, 2, 6, 3, 0), SignMode.Flat),
    Ges -> Sign(Vector(3, 0, 4, 1, 5, 2), SignMode.Flat),
    fes -> Sign(Vector(5, 2, 6, 3, 0, 4), SignMode.Flat))
}

object ScaleType extends Enumeration {
  val Major, Minor, Melodic, Harmonic, PentatonicMinorBlues, PentatonicMajorBlues, Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian, PentatonicMinor, PentatonicMajor = Value

  val scaleVector = Map[ScaleType.Value, Vector[Int]](
    Major -> Vector(0, 2, 4, 5, 7, 9, 11, 12),
    Minor -> Vector(0, 2, 3, 5, 7, 8, 10, 12),
    Melodic -> Vector(0, 2, 3, 5, 7, 9, 11, 12),
    Harmonic -> Vector(0, 2, 3, 5, 7, 8, 11, 12),
    PentatonicMinorBlues -> Vector(0, 3, 5, 6, 7, 10, 12),
    PentatonicMajorBlues -> Vector(0, 2, 3, 4, 7, 9, 12),
    Ionian -> Vector(0, 2, 4, 5, 7, 9, 11, 12),
    Dorian -> Vector(0, 2, 3, 5, 7, 9, 10, 12),
    Phrygian -> Vector(0, 1, 3, 5, 7, 8, 10, 12),
    Lydian -> Vector(0, 2, 4, 6, 7, 9, 11, 12),
    Mixolydian -> Vector(0, 2, 4, 5, 7, 9, 10, 12),
    Aeolian -> Vector(0, 2, 3, 5, 7, 8, 10, 12),
    Locrian -> Vector(0, 1, 3, 5, 6, 8, 10, 12),
    PentatonicMinor -> Vector(0, 3, 5, 7, 10, 12),
    PentatonicMajor -> Vector(0, 2, 4, 7, 9, 12))
}
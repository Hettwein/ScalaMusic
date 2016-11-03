package de.htwg

import de.htwg.music.elements._

package object music {
  
  val C, c = Note(pitch = 60)
  val D, d = Note(pitch = 62)
  val E, e = Note(pitch = 64)
  val F, f = Note(pitch = 65)
  val G, g = Note(pitch = 67)
  val A, a = Note(pitch = 69)
  val B, b = Note(pitch = 71)
  
  val Cis, cis = c.sharp
  val Dis, dis = d.sharp
  val Eis, eis = e.sharp
  val Fis, fis = f.sharp
  val Gis, gis = g.sharp
  val Ais, ais = a.sharp
  val Bis, bis = b.sharp

  val Ces, ces = c.flat
  val Des, des = d.flat
  val Es, es = e.flat
  val Fes, fes = f.flat
  val Ges, ges = g.flat
  val As, as = a.flat
  val Bes, bes = b.flat
  
  val r1 = Rest(1)
  val r2 = Rest(2)
  val r4 = Rest(4)
  val r8 = Rest(8)
  val r16 = Rest(16)
}
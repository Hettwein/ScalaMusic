package de.htwg.scalamusic

import scala.io.StdIn
import de.htwg.scalamusic.parser._
import de.htwg.scalamusic._

object Main extends App {
  println("Welcome to ScalaMusic!")
  help()
  var running = true
  while (running) {
    StdIn.readLine("Please choose an option.\n") match {
      case "m" =>
        val s = DSLParser(read); if (s != null) println(s.asDSL)
      case "show" =>
        val s = DSLParser(read); if (s != null) ShowAsLy(s)
      case "ly" =>
        val s = DSLParser(read); if (s != null) println(ShowAsLy.generateLy(s))
      case "generate" =>
        val s = DSLParser(read); if (s != null) ShowAsLy(new BasslineGenerator(s).generate)
      case "?" => help()
      case "q" => running = false
      case _ => println("Unknown option.")
    }
  }

  def help() = println("\n-m: show as full DSL-String\n-ly: show as LilyPond-String \n-show: create sheet of music and midi file\n-generate: add a generated bassline matching chords and create sheet of music and midi file\n-?: help\n-q: quit\n")
  def read(): String = StdIn.readLine("Now enter your music.\n")
}
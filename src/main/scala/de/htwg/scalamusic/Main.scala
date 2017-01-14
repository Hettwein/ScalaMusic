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
      case "m" => println(DSLParser(read).asDSL)
      case "show" => ShowAsLy(DSLParser(read))
      case "ly" => println(ShowAsLy.generateLy(DSLParser(read)))
      case "generate" => ShowAsLy(new BasslineGenerator(DSLParser(read)).generate)
      case "?" => help()
      case "q" => running = false
      case _ => println("Unkown option.")
    }
  }

  def help() = println("\n-m: show as full DSL-String\n-ly: show as LilyPond-String \n-show: create sheet of music and midi file\n-generate: add a generated bassline matching chords and create sheet of music and midi file\n-?: help\n-q: quit\n")
  def read(): String = StdIn.readLine("Now enter your music.\n")
}
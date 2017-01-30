package de.htwg.scalamusic

import scala.io._
import de.htwg.scalamusic.parser._
import de.htwg.scalamusic._
import java.io.File

object Main extends App {
  println("Welcome to ScalaMusic!")
  var running = true
  while (running) {
    val fn = StdIn.readLine("Please enter a file name (with file extension) or press q to quit\n")
    if (fn != "q") {
      if (new File(fn).exists) {
        val input = fileRead(Source.fromFile(fn));
        var workingOnFile = true
        while (workingOnFile) {
          StdIn.readLine("Please choose an option:\n" + help()) match {
            case "m" =>
              val s = DSLParser(input); if (s != null) println("\n" + s.asDSL + "\n")
            case "show" =>
              val s = DSLParser(input); if (s != null) ShowAsLy(s, fn.split('.')(0))
            case "ly" =>
              val s = DSLParser(input); if (s != null) println("\n" + ShowAsLy.generateLy(s) + "\n")
            case "generate" =>
              val s = DSLParser(input); if (s != null) ShowAsLy(new BasslineGenerator(s).generate, fn.split(Array('\\', '/')).last.split('.')(0))
            case "?" => println(help())
            case "q" =>
              workingOnFile = false; if (StdIn.readLine("Do you want to work with another file? (y / n)\n") != "y") running = false
            case _ => println("Unknown option.")
          }
        }
      } else {
        println("File does not exist!")
      }
    } else {
      running = false
    }
  }

  def help(): String = "\nm: show as full DSL-String\nly: show as LilyPond-String \nshow: create sheet of music and midi file\ngenerate: add generated bassline and create output files\n?: help\nq: quit\n"
  def read(): String = StdIn.readLine("Now enter your music.\n")
  def fileRead(file: Source): String = {
    try file.getLines mkString " " finally file.close()
  }
}
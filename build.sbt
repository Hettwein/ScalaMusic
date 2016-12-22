name          := "ScalaMusic"
organization  := "de.htwg"
version       := "0.0.1"
scalaVersion  := "2.11.8"
scalacOptions := Seq("-unchecked", "-feature", "-deprecation", "-encoding", "utf8")

initialCommands in console := """
import de.htwg.scalamusic._
import de.htwg.scalamusic.parser._
"""

libraryDependencies ++= {
  val scalaTestV       = "3.0.0-M15"
  val scalaMockV       = "3.2.2"
  Seq("de.sciss" %% "scalamidi" % "0.2.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.scalatest" %% "scalatest"                   % scalaTestV       % "test",
  "org.scalamock" %% "scalamock-scalatest-support" % scalaMockV       % "test"
  )
}

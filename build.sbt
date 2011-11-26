name := "scalaaudiofile"

version := "0.20"

organization := "de.sciss"

scalaVersion := "2.9.1"

crossScalaVersions := Seq("2.9.1", "2.9.0", "2.8.1")

description := "A library to read and write uncompressed audio files (AIFF, WAVE, etc.)"

homepage := Some( url( "https://github.com/Sciss/ScalaAudioFile" ))

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

// fix sbt issue #85 (https://github.com/harrah/xsbt/issues/85)
unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))

// ---- publishing ----

publishTo <<= version { (v: String) =>
   Some( "Scala Tools Nexus" at "http://nexus.scala-tools.org/content/repositories/".+(
      if( v.endsWith( "-SNAPSHOT")) "snapshots/" else "releases/"
   ))
}

pomExtra :=
<licenses>
  <license>
    <name>GPL v2+</name>
    <url>http://www.gnu.org/licenses/gpl-2.0.txt</url>
    <distribution>repo</distribution>
  </license>
</licenses>

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

// ---- ls.implicit.ly ----

seq( lsSettings :_* )

(LsKeys.tags in LsKeys.lsync) := Seq( "audio-file", "audio", "sound-file", "sound", "dsp" )

(LsKeys.ghUser in LsKeys.lsync) := Some( "Sciss" )

(LsKeys.ghRepo in LsKeys.lsync) := Some( "ScalaAudioFile" )

// bug in ls -- doesn't find the licenses from global scope
(licenses in LsKeys.lsync) := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

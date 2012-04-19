name := "ScalaAudioFile"

version := "0.20"

organization := "de.sciss"

scalaVersion := "2.9.2"

// crossScalaVersions := Seq("2.9.1", "2.9.0", "2.8.1")

description := "A library to read and write uncompressed audio files (AIFF, WAVE, etc.)"

homepage := Some( url( "https://github.com/Sciss/ScalaAudioFile" ))

licenses := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

// fix sbt issue #85 (https://github.com/harrah/xsbt/issues/85)
unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))

// ---- publishing ----

publishMavenStyle := true

publishTo <<= version { (v: String) =>
   Some( if( v.endsWith( "-SNAPSHOT" ))
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
   else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
   )
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra :=
<scm>
  <url>git@github.com:Sciss/ScalaAudioFile.git</url>
  <connection>scm:git:git@github.com:Sciss/ScalaAudioFile.git</connection>
</scm>
<developers>
   <developer>
      <id>sciss</id>
      <name>Hanns Holger Rutz</name>
      <url>http://www.sciss.de</url>
   </developer>
</developers>

// ---- ls.implicit.ly ----

seq( lsSettings :_* )

(LsKeys.tags in LsKeys.lsync) := Seq( "audio-file", "audio", "sound-file", "sound", "dsp" )

(LsKeys.ghUser in LsKeys.lsync) := Some( "Sciss" )

(LsKeys.ghRepo in LsKeys.lsync) := Some( "ScalaAudioFile" )

// bug in ls -- doesn't find the licenses from global scope
(licenses in LsKeys.lsync) := Seq( "GPL v2+" -> url( "http://www.gnu.org/licenses/gpl-2.0.txt" ))

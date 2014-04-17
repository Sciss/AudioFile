name               := "ScalaAudioFile"

version            := "1.4.2"

organization       := "de.sciss"

scalaVersion       := "2.11.0"

crossScalaVersions := Seq("2.11.0", "2.10.4")

description        := "A library to read and write uncompressed audio files (AIFF, WAVE, etc.)"

homepage           := Some(url("https://github.com/Sciss/" + name.value))

licenses           := Seq("LGPL v3+" -> url("http://www.gnu.org/licenses/lgpl-3.0.txt"))

initialCommands in console := """import de.sciss.synth.io._"""

libraryDependencies ++= Seq(
  "de.sciss"      %% "serial"    % "1.0.2",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test"
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture")

// ---- build info ----

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
  BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
  BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
)

buildInfoPackage := "de.sciss.synth.io"

// ---- publishing ----

publishMavenStyle := true

publishTo :=
  Some(if (version.value endsWith "-SNAPSHOT")
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := { val n = name.value
<scm>
  <url>git@github.com:Sciss/{n}.git</url>
  <connection>scm:git:git@github.com:Sciss/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
}

// ---- ls.implicit.ly ----

seq(lsSettings :_*)

(LsKeys.tags   in LsKeys.lsync) := Seq("audio-file", "audio", "sound-file", "sound", "dsp")

(LsKeys.ghUser in LsKeys.lsync) := Some("Sciss")

(LsKeys.ghRepo in LsKeys.lsync) := Some(name.value)

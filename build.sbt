name := "scalaaudiofile"

version := "0.17"

organization := "de.sciss"

scalaVersion := "2.9.1"

crossScalaVersions := Seq("2.9.1", "2.9.0", "2.8.1")

// fix sbt issue #85 (https://github.com/harrah/xsbt/issues/85)
unmanagedClasspath in Compile += Attributed.blank(new java.io.File("doesnotexist"))

// ---- publishing ----

publishTo := Some(ScalaToolsReleases)

pomExtra :=
<licenses>
  <license>
    <name>GPL v2+</name>
    <url>http://www.gnu.org/licenses/gpl-2.0.txt</url>
    <distribution>repo</distribution>
  </license>
</licenses>

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")


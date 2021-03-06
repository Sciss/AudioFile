lazy val baseName  = "AudioFile"
lazy val baseNameL = baseName.toLowerCase

lazy val projectVersion = "2.3.3"
lazy val mimaVersion    = "2.3.0"

lazy val deps = new {
  val main = new {
    val asyncFile = "0.1.3"
    val dom       = "1.1.0"
    val log       = "0.1.1"
    val serial    = "2.0.1"
  }
  val test = new {
    val scalaTest = "3.2.9"
  }
}

lazy val commonJvmSettings = Seq(
  crossScalaVersions := Seq("3.0.0", "2.13.5", "2.12.13"),
)

// sonatype plugin requires that these are in global
ThisBuild / version      := projectVersion
ThisBuild / organization := "de.sciss"

lazy val root = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .jvmSettings(commonJvmSettings)
  .settings(
    name               := baseName,
//    version            := projectVersion,
//    organization       := "de.sciss",
    scalaVersion       := "2.13.5",
    description        := "A library to read and write uncompressed audio files (AIFF, WAVE, etc.)",
    homepage           := Some(url(s"https://github.com/Sciss/${name.value}")),
    licenses           := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
    mimaPreviousArtifacts := Set("de.sciss" %% baseNameL % mimaVersion),
    initialCommands in console := """import de.sciss.audiofile._""",
    libraryDependencies ++= Seq(
      "de.sciss"      %%% "asyncfile" % deps.main.asyncFile,
      "de.sciss"      %%% "log"       % deps.main.log,
      "de.sciss"      %%% "serial"    % deps.main.serial,
      "org.scalatest" %%% "scalatest" % deps.test.scalaTest % Test,
    ),
    scalacOptions ++= Seq(
      "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xlint", "-Xsource:2.13",
    ),
    scalacOptions in (Compile, compile) ++= {
      val jdkGt8  = scala.util.Properties.isJavaAtLeast("9")
      val sv      = scalaVersion.value
      // val dot     = isDotty.value  // https://github.com/lampepfl/dotty/issues/8634
      val sq0     = (if (/* !dot && */ jdkGt8) List("-release", "8") else Nil)
      if (sv.startsWith("2.12.")) sq0 else "-Wvalue-discard" :: sq0
    }, // JDK >8 breaks API; skip scala-doc
    // ---- build info ----
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
    ),
    buildInfoPackage := "de.sciss.audiofile"
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % deps.main.dom,
    ),
  )
  .settings(publishSettings)

// ---- publishing ----
lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  developers := List(
    Developer(
      id    = "sciss",
      name  = "Hanns Holger Rutz",
      email = "contact@sciss.de",
      url   = url("https://www.sciss.de")
    )
  ),
  scmInfo := {
    val h = "git.iem.at"
    val a = s"sciss/${name.value}"
    Some(ScmInfo(url(s"https://$h/$a"), s"scm:git@$h:$a.git"))
  },
)

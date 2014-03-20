package de.sciss.synth.io

object IdentifyTest extends App {
  args.foreach { path =>
    println(if (AudioFile.identify(path).isDefined) {
      AudioFile.readSpec(path).toString
    } else {
      "Unknown file type : " + path
    })
  }
}

package de.sciss.synth.io

object ReadSpeedApp extends App {
  val DirectMemory = args.headOption == Some("--direct")

  sys.props.put("de.sciss.synth.io.AudioFile.DirectMemory", DirectMemory.toString)
  val path  = "/home/hhrutz/Documents/misc/HMS/rec/HMSChristmas141225_Set1_Sciss.aif" // c. 1 GB file
  println(s"Reading file (direct-memory = $DirectMemory)...")
  val t1    = System.currentTimeMillis()
  val af    = AudioFile.openRead(path)
  val bsz   = 8192
  val buf   = af.buffer(bsz)
  var off   = 0L
  val N     = af.numFrames
  while (off < N) {
    val len = math.min(bsz, N - off).toInt
    af.read(buf, 0, len)
    off += len
  }
  af.close()
  val t2 = System.currentTimeMillis()
  println(f"Took ${(t2 - t1).toDouble/1000}%1.3f seconds.")
}

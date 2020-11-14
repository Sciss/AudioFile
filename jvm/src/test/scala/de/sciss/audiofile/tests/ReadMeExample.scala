package de.sciss.audiofile.tests

object ReadMeExample {
  import de.sciss.audiofile._

  val in      = AudioFile.openRead("input.aif")
  // for the output, switch to AIFF 24-bit integer,
  // but keep the other parameters (sample-rate and number-of-channels)
  val outSpec = in.spec.copy(fileType = AudioFileType.AIFF,
    sampleFormat = SampleFormat.Int24)
  val out     = AudioFile.openWrite("output.aif", outSpec)

  // create a buffer
  val bufSz   = 8192  // perform operations in blocks of this size
  val buf     = in.buffer(bufSz)

  // first pass: determine maximum magnitude
  var mag     = 0.0
  var remain  = in.numFrames
  while (remain > 0) {
    val chunk = math.min(bufSz, remain).toInt
    in.read(buf, 0, chunk)
    buf.foreach { chan =>
      mag = math.max(mag, math.abs(chan.maxBy(math.abs)))
    }
    remain -= chunk
  }
  println(f"Maximum magnitude detected: $mag%1.3f")

  // second pass: adjust gain and write output
  require(mag > 0)
  val gain = 1.0 / mag
  in.seek(0L) // start over from the beginning of the file
  remain = in.numFrames
  while (remain > 0) {
    val chunk = math.min(bufSz, remain).toInt
    in.read(buf, 0, chunk)
    buf.foreach { chan =>
      for (i <- 0 until chunk) {
        chan(i) *= gain
      }
    }
    out.write(buf, 0, chunk)
    remain -= chunk
  }
  out.close()
  in.close()
}

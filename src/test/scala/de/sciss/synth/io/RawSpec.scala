package de.sciss.synth.io

/*
  testOnly de.sciss.synth.io.RawSpec
 */
class RawSpec extends TempFileSpec {
  "AudioFile" should "write and read raw files" in { f =>
    val spec    = AudioFileSpec(AudioFileType.Raw, SampleFormat.Float, numChannels = 1, sampleRate = 44100)
    val afOut   = AudioFile.openWrite(f, spec)
    val bufOut  = afOut.buffer(3)
    bufOut(0)(0) = 0.1f
    bufOut(0)(1) = 0.2f
    bufOut(0)(2) = -0.3f
    try {
      afOut.write(bufOut)
    } finally {
      afOut.close()
    }

    val raw = AudioFileType.Raw.reader(spec)
    val afIn = raw.openRead(f)
    try {
      assert(afIn.numFrames     === 3L)
      assert(afIn.numChannels   === 1)
      assert(afIn.sampleFormat  === SampleFormat.Float)
      assert(afIn.sampleRate    === 44100.0)

      val bufIn = afIn.buffer(3)
      afIn.read(bufIn)
      assert(bufIn === bufOut)

    } finally {
      afIn.close()
    }
  }
}
package de.sciss.audiofile

class Issue5 extends TempFileSpec {
  "Wave64" should "correctly report numFrames" in { f =>
    val data = Array.ofDim[Double](1, 1)
    val afOut = AudioFile.openWrite(f, AudioFileSpec(AudioFileType.Wave64, SampleFormat.Int24, numChannels = 1,
      sampleRate = 48000.0))
    try {
      data(0)(0) = 0.1234
      afOut.write(data)
    } finally {
      afOut.cleanUp()
    }

    val afIn = AudioFile.openRead(f)
    try {
      assert (afIn.numChannels === 1)
      assert (afIn.numFrames === 1)
      assert (afIn.sampleRate === 48000.0)

      afIn.read(data)
      assert (data(0)(0) === 0.1234 +- 1.0e-4)

    } finally {
      afIn.cleanUp()
    }
  }
}

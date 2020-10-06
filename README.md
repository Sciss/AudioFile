# AudioFile

[![Build Status](https://travis-ci.org/Sciss/AudioFile.svg?branch=main)](https://travis-ci.org/Sciss/AudioFile)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/audiofile_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/audiofile_2.13)

## statement

AudioFile is a Scala library to read and write audio files. It is (C)opyright 2004&ndash;2020 by Hanns Holger Rutz.
All rights reserved. AudioFile is released under
the [GNU Lesser General Public License](https://git.iem.at/sciss/AudioFile/raw/main/LICENSE) v2.1+ and comes
with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

(Note: the project and artifacts were formerly called **Scala**AudioFile.)

## requirements / installation

AudioFile currently compiles against Scala 2.13, 2.12 using sbt. The last version to support Scala 2.11 was v1.5.3.

To use the library in your project:

    "de.sciss" %% "audiofile" % v

The current version `v` is `"2.0.1"`

## contributing

Please see the file [CONTRIBUTING.md](CONTRIBUTING.md)

## supported formats

|**name**        |**read/write**   |**notes**      |
|----------------|-----------------|---------------|
|AIFF            |R/W              |               |
|WAVE            |R/W              |               |
|IRCAM           |R/W              |aka BICSF      |
|NeXT            |R/W              |aka Snd and AU |
|Wave64          |R/W              |               |
|Raw             |R/W              |headerless     |

## getting started

AudioFile currently supports reading and writing files, while still lacking a few features of the Java predecessor `de.sciss.io.AudioFile`
from [ScissLib](http://sourceforge.net/projects/scisslib), such as reading and writing markers and application specific chunks.

* To open an audio file for __reading__: `AudioFile.openRead(aFile)` or `AudioFile.openRead(anInputStream)`. The `InputStream` variant has limited functionality, e.g. you cannot seek into the file, but only read sequentially. The stream variant can be used to decode files from an HTTP connection or in-memory (`ByteArrayInputStream`).
* To just retrieve the __specification__ of an existing file, to see if it can be decoded, what is length, number of channels and format are: `AudioFile.readSpec(fileOrPathName)`.
* To open a file for __writing__: `AudioFile.openWrite(aFile, spec)` or `AudioFile.openWrite(anOutputStream, spec)`

To the user, __frame data__ is always represented as de-interleaved 32-bit floating point data, so you create a user buffer through `Array.ofDim[Float](numChannels, bufFrames)`, or use a convenience method such as `AudioFile.buffer(...)`.

The `AudioFile` implementation is currently not thread-safe, but synchronization is planned. At the moment, make sure you are not sharing an instance across threads. Alternatively, use a lock and validate the frame position before each read/write.

Here is an example of opening an existing file, reading through it to determine the maximum sample magnitude, then writing a normalized version to a new file:

```scala
    
    import de.sciss.synth.io._
    
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
    var mag     = 0f
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
    val gain = 1f / mag
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
    
```

## properties

There is a system property `de.sciss.synth.io.AudioFile.DirectMemory` whose value can be set to `true` in order 
to use direct-memory buffers (`ByteBuffer.allocateDirect`). Reading in a 1 GB is around 6% faster with direct 
memory. Direct memory can have the disadvantage that it requires an individual VM switch to adjust the maximum 
available memory before throwing an out-of-memory error. The default is __false__.

For all further information, please refer to the API docs. They can be created with `sbt doc`.

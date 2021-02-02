# AudioFile

[![Build Status](https://github.com/Sciss/AudioFile/workflows/Scala%20CI/badge.svg?branch=main)](https://github.com/Sciss/AudioFile/actions?query=workflow%3A%22Scala+CI%22)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/de.sciss/audiofile_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/de.sciss/audiofile_2.13)

## statement

AudioFile is a Scala library to read and write audio files. It is (C)opyright 2004&ndash;2021 by Hanns Holger Rutz.
All rights reserved. AudioFile is released under
the [GNU Affero General Public License](https://git.iem.at/sciss/AudioFile/raw/main/LICENSE) v3+ and comes
with absolutely no warranties. To contact the author, send an e-mail to `contact at sciss.de`.

## requirements / installation

AudioFile builds with sbt against Scala 2.13, 2.12, Dotty (JVM) and Scala 2.13 (JS).
The last version to support Scala 2.11 was v1.5.3.

To use the library in your project:

    "de.sciss" %% "audiofile" % v

The current version `v` is `"2.3.3"`

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

AudioFile currently supports reading and writing files in compressed (professional) audio formats such as AIFF,
Wave or Wave64. It focuses on the audio data, while currently not supporting meta-data features such as
markers, regions, or application specific data stored in the file headers.

The original API was made for synchronous I/O on the JVM:

- To open an audio file for __reading__: `AudioFile.openRead(aFile)` or `AudioFile.openRead(anInputStream)`. 
  The `InputStream` variant has limited functionality, e.g. you cannot seek into the file, but only read 
  sequentially. The stream variant can be used to decode files from an HTTP connection or
  in-memory (`ByteArrayInputStream`).
- To just retrieve the __specification__ of an existing file, to see if it can be decoded, what is length, number of
  channels and format are: `AudioFile.readSpec(fileOrPathName)`.
- To open a file for __writing__: `AudioFile.openWrite(aFile, spec)` or `AudioFile.openWrite(anOutputStream, spec)`

More recently, an asynchronous I/O API was added, which is the only API available on Scala.js, as the JavaScript
virtual machine does not allow synchronous I/O. On the JVM, synchronous I/O is slightly faster and allows for a much
simpler control flow (no need to map futures).

To the user, __frame data__ is always represented as de-interleaved 64-bit floating point data, so you create a user 
buffer through `Array.ofDim[Float](numChannels, bufFrames)`, or use a convenience method such 
as `AudioFile.buffer(...)`. In other words, all sample frames are mapped from their native `SampleFormat` such as
16-bit integer to floating-point numbers in the range -1 to +1. Integers up to 32-bit can be represented this way
without loss of precision. In the future, we might support the native storage format and/or 32-bit floating point
data (like it used to be before version 2.3.0).

The synchronous `AudioFile` implementation is currently not thread-safe, so one should use a single file only from
within the same thread. Alternatively, use a lock and validate the frame position before each read/write. The
asynchronous API should be fairly thread-safe on the JVM; currently it is not permitted to run more than a single
read or write operation at a time.

Here is an example of opening an existing file with synchronous I/O, reading through it to determine the maximum 
sample magnitude, then writing a normalized version to a new file:

```scala
    
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

```

## Scala.js

Scala.js does not know the type `java.io.File` and does not support synchronous I/O. The asynchronous I/O API uses
abstractions `AsyncReadableByteChannel` and `AsyncWriteableByteChannel` which are modelled after NIO's
asynchronous byte and file channels. Files are opened with `openReadAsync` and `openWriteAsync`, using a `URI`.
We currently implement a virtual file system with schema `idb` for IndexedDB, i.e. for accessing and storing
files in a website's client side browser cache.

## properties

There is a system property `AudioFile.DirectMemory` whose value can be set to `true` in order 
to use direct-memory buffers (`ByteBuffer.allocateDirect`). Reading in a 1.6 GB is around 18% faster with direct 
memory. Direct memory can have the disadvantage that it requires an individual VM switch to adjust the maximum 
available memory before throwing an out-of-memory error. The default is __false__.

Using an asynchronous file I/O is around 26% slower than synchronous I/O (on JVM).

For all further information, please refer to the API docs. They can be created with `sbt doc`.

## change log

- v2.2.0: adds Scala.js support and asynchronous API. The base package was renamed from 
  `de.sciss.synth.io` to `de.sciss.audiofile`.

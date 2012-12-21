## ScalaAudioFile

### statement

ScalaAudioFile is a Scala library to read and write audio files. It is (C)opyright 2004-2012 by Hanns Holger Rutz. All rights reserved. ScalaAudioFile is released under the [GNU General Public License](https://raw.github.com/Sciss/ScalaAudioFile/master/licenses/ScalaAudioFile-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

### requirements / installation

ScalaAudioFile currently compiles against Scala 2.9.2 using sbt 0.12.

To use the library in your project:

    "de.sciss" %% "scalaaudiofile" % "1.2.+"

### supported formats

|**name**        |**read/write**   |**notes**      |
|----------------|-----------------|---------------|
|AIFF            |R/W              |               |
|WAVE            |R/W              |               |
|IRCAM           |R/W              |aka BICSF      |
|NeXT            |R/W              |aka Snd and AU |
|Wave64          |-                |               |
|Raw             |R/W              |headerless     |

### getting started

ScalaAudioFile currently supports reading and writing files, while still lacking a few features of the Java predecessor `de.sciss.io.AudioFile`
from [ScissLib](http://sourceforge.net/projects/scisslib), such as reading and writing markers and application specific chunks.

* To open an audio file for __reading__: `AudioFile.openRead( aFile )` or `AudioFile.openRead( anInputStream )`. The `InputStream` variant has limited functionality, e.g. you cannot seek into the file, but only read sequentially. The stream variant can be used to decode files from an HTTP connection or in-memory (`ByteArrayInputStream`).
* To just retrieve the __specification__ of an existing file, to see if it can be decoded, what is length, number of channels and format are: `AudioFile.readSpec( fileOrPathName )`.
* To open a file for __writing__: `AudioFile.openWrite( aFile, spec )` or `AudioFile.openWrite( anOutputStream, spec )`

To the user, __frame data__ is always represented as de-interleaved 32-bit floating point data, so you create a user buffer through `Array.ofDim[ Float ]( numChannels, bufFrames )`, or use a convenience method such as `AudioFile.frameBuffer(...)`.

The `AudioFile` implementation is currently not thread-safe, but synchronization is planned. At the moment, make sure you are not sharing an instance across threads.

Here is an example of opening an existing file, reading through it to determine the maximum sample magnitude, then writing a normalized version to a new file:

```scala
    
    import de.sciss.synth.io._
    
    val in      = AudioFile.openRead( "input.aif" )
    // for the output, switch to AIFF 24-bit integer, 
    // but keep the other parameters (sample-rate and number-of-channels)
    val outSpec = in.spec.copy( fileType = AudioFileType.AIFF, 
                                sampleFormat = SampleFormat.Int24 )
    val out     = AudioFile.openWrite( "output.aif", outSpec )
    
    // create a buffer
    val bufSz   = 8192  // perform operations in blocks of this size
    val buf     = in.buffer( bufSz )

    // first pass: determine maximum magnitude
    var mag     = 0f
    var remain  = in.numFrames
    while( remain > 0 ) {
       val chunk = math.min( bufSz, remain )
       in.read( buf )
       buf.foreach { chan =>
          mag = math.max( mag, chan.maxBy( math.abs( _ )))
       }
       remain -= chunk
    }
    println( "Maximum magnitude detected: " + mag )

    // second pass: adjust gain and write output
    require( mag > 0.0 )
    val gain = 1f / mag
    in.seek( 0L ) // start over from the beginning of the file
    remain = in.numFrames
    while( remain > 0 ) {
       val chunk = math.min( bufSz, remain )
       in.read( buf )
       buf.foreach { chan =>
          for( i <- 0 until chunk ) {
             chan( i ) *= gain
          }
       }
       out.write( buf )
       remain -= chunk
    }
    out.close()
    in.close()
    
```

For all further information, please refer to the API docs. They can be created with `sbt doc`.

### creating an IntelliJ IDEA project

If you want to develop the library, you can set up an IntelliJ IDEA project, using the sbt-idea plugin yet. Have the following contents in `~/.sbt/plugins/build.sbt`:

    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

Then to create the IDEA project, run `sbt gen-idea`.

### download

The current version can be downloaded from [github.com/Sciss/ScalaAudioFile](http://github.com/Sciss/ScalaAudioFile)

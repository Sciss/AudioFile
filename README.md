## ScalaAudioFile

### statement

ScalaAudioFile is a Scala library to read and write audio files. It is (C)opyright 2004-2011 by Hanns Holger Rutz. All rights reserved. ScalaAudioFile is released under the [GNU General Public License](https://raw.github.com/Sciss/ScalaAudioFile/master/licenses/ScalaAudioFile-License.txt) and comes with absolutely no warranties. To contact the author, send an email to `contact at sciss.de`

### requirements / installation

ScalaAudioFile currently compiles against Scala 2.9.1 (default), 2.9.0 and 2.8.1. It builds with sbt 0.11 (xsbt), with the standard targets:

* `clean` &ndash; removes previous build artefacts
* `compile` &ndash; compiles classes into target/scala-version/classes
* `doc` &ndash; generates api in target/scala-version/api/index.html
* `package` &ndash; packages jar in target/scala-version
* `console` &ndash; runs the Scala REPL with ScalaAudioFile on the class path

### creating an IntelliJ IDEA project

The IDEA project files have now been removed from the git repository, but they can be easily recreated, given that you have installed the sbt-idea plugin. If you haven't yet, create the following contents in `~/.sbt/plugins/build.sbt`:

    resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
    
    addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "0.11.0")

Then to create the IDEA project, run the following two commands from the xsbt shell:

    > set ideaProjectName := "ScalaAudioFile"
    > gen-idea

### getting started

ScalaAudioFile is a refactored version of the Java class `de.sciss.io.AudioFile` from [ScissLib](http://sourceforge.net/projects/scisslib). The number of supported formats is currently still smaller, and some functionality is still missing (e.g. reading and writing markers and app-chunks).

Currently supported are AIFF (read, write), WAVE (read), and IRCAM (read, write)! I hope to add WAVE write and Wave64 read/write soon.

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

For all further information, please refer to the API docs. They can be created with `xsbt doc`.

### download

The current version can be downloaded from [github.com/Sciss/ScalaAudioFile](http://github.com/Sciss/ScalaAudioFile)

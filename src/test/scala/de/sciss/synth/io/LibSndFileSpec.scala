package de.sciss.synth.io

import java.io.File

/** Assumes that `sndfile` (part of libsndfile) is available on the system and found in a standard
  * `$PATH`. Generates sound files which are then fed through libsndfile (tested with v1.0.25),
  * checking the printed output for detected format.
  *
  * To run only this test:
  * test-only de.sciss.synth.io.LibSndFileSpec
  */
class LibSndFileSpec extends TempFileSpec {
  val rwTypes = AudioFileType.readable.collect {
    case cw: AudioFileType.CanWrite => cw
  }

  val chanNums = List(1, 2, 3)

  val bufSize   = 8192
  val totalSize = 10000
  val size2     = totalSize - bufSize
  val sr        = 44100.0

   def isSupportedByLibSndFile( spec: AudioFileSpec ) : Boolean = spec match {
      case AudioFileSpec( AudioFileType.IRCAM, SampleFormat.Int24,   _, _, _, _ ) => false
      case AudioFileSpec( AudioFileType.IRCAM, SampleFormat.Int8,    _, _, _, _ ) => false
      case AudioFileSpec( AudioFileType.IRCAM, SampleFormat.Double,  _, _, _, _ ) => false
      case _ => true
   }

   val numFrames = 10000L

   val sndFileInfo   = "/usr/local/bin/sndfile-info"
   if( !new File( sndFileInfo ).canExecute ) {
      println( "Error: libsndfile not found (" + sndFileInfo + "); skipping these tests." )

   } else {
      rwTypes.foreach { typ =>
         typ.supportedFormats.foreach { smpFmt =>
            chanNums.foreach { numCh =>
               val fileSpec = AudioFileSpec( typ, smpFmt, numCh, sr )
               if( isSupportedByLibSndFile( fileSpec )) {
                  "libsndfile" should ("understand " + fileSpec) in { f =>
                     val afOut   = AudioFile.openWrite( f, fileSpec )
                     val buf     = afOut.buffer( 8192 )
                     var left    = numFrames
                     while( left > 0 ) {
                        val chunkLen = math.min( 8192, left ).toInt
                        afOut.write( buf, 0, chunkLen )
                        left -= chunkLen
                     }
                     afOut.close()

                     import sys.process._
                     val cmd  = sndFileInfo :: f.getPath :: Nil
                     val txt  = cmd.!!.split( '\n' ).toIndexedSeq

                     // example:
                     // ...
                     // ----------------------------------------
                     // Sample Rate : 44100
                     // Frames      : 1024
                     // Channels    : 3
                     // Format      : 0x100A0006
                     // ...

                     val txtSr   = txt.find( _.contains( "Sample Rate" )).get
                     val srFound = txtSr.substring( txtSr.indexOf( ':' ) + 1 ).trim.toDouble
                     assert( srFound === afOut.sampleRate )
                     val txtFr   = txt.find( _.contains( "Frames" )).get
                     val frFound = txtFr.substring( txtFr.indexOf( ':' ) + 1 ).trim.toLong
                     assert( frFound === afOut.numFrames )
                     val txtCh   = txt.find( _.contains( "Channels" )).get
                     val chFound = txtCh.substring( txtCh.indexOf( ':' ) + 1 ).trim.toInt
                     assert( chFound === afOut.numChannels )
                     val txtForm = txt.find( _.contains( "Format" )).get

                     if( typ == AudioFileType.Wave || typ == AudioFileType.Wave64 ) {
                        // sndfile-info prints either "0x1 => WAVE_FORMAT_PCM" or "0x3 => WAVE_FORMAT_IEEE_FLOAT"
                        val formFound     = txtForm.substring( txtForm.indexOf( ':' ) + 1 ).trim
                        val formExpected  = if( smpFmt == SampleFormat.Float || smpFmt == SampleFormat.Double ) {
                           "0x3 => WAVE_FORMAT_IEEE_FLOAT"
                        } else {
                           "0x1 => WAVE_FORMAT_PCM"
                        }
                        assert( formFound === formExpected )

                     } else {
                        // sndfile-info prints a hexstring
                        val formFound = java.lang.Integer.parseInt( txtForm.substring( txtForm.indexOf( "0x" ) + 2 ).trim, 16 )

                        val formExpected = (typ match {
                           case AudioFileType.Wave    => 0x010000
                           case AudioFileType.AIFF    => 0x020000
                           case AudioFileType.NeXT    => 0x030000
                           case AudioFileType.IRCAM   => 0x0A0000
                           case AudioFileType.Wave64  => 0x0B0000
                        }) | (smpFmt match {
                           case SampleFormat.Int8     => 0x0001
                           case SampleFormat.Int16    => 0x0002
                           case SampleFormat.Int24    => 0x0003
                           case SampleFormat.Int32    => 0x0004
                           case SampleFormat.UInt8    => 0x0005
                           case SampleFormat.Float    => 0x0006
                           case SampleFormat.Double   => 0x0007
                        })

                        // note: libsndfile often uses 0x00000000 for the byte order (i.e. 'default').
                        // we simply filter out the byte order check.

   //                     | (afOut.spec.byteOrder match {
   //                        case Some( ByteOrder.LITTLE_ENDIAN )   => 0x10000000
   //                        case Some( ByteOrder.BIG_ENDIAN )      => 0x20000000
   //                        case None                              => 0x00000000
   //                     })
   //                     assert( formFound === formExpected )

                        assert( (formFound & 0xFFFFFFF) === (formExpected & 0xFFFFFFF) )
                     }
                  }
               }
            }
         }
      }
   }
}
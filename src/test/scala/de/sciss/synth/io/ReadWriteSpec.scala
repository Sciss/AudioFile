package de.sciss.synth.io

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.fixture
import java.io.File

class ReadWriteSpec extends fixture.FlatSpec with ShouldMatchers {
   final type FixtureParam = File

   final def withFixture( test: OneArgTest ) {
      val f = File.createTempFile( "tmp", ".bin" )
      try {
         test( f )
      }
      finally {
         if( !f.delete() ) f.deleteOnExit()
      }
   }

   val rwTypes = AudioFileType.readable.collect {
      case cw: AudioFileType.CanWrite => cw
   }

   val chanNums = List( 1, 2, 3 )

   // XXX TODO : generate actual buffer content, and verify it

   val bufSize    = 8192
   val totalSize  = 10000
   val sr         = 44100.0

   rwTypes.foreach { typ =>
      typ.supportedFormats.foreach { smpFmt =>
         chanNums.foreach { numCh =>
            val fileSpec = AudioFileSpec( typ, smpFmt, numCh, sr )
            "AudioFile" should ("write and read " + fileSpec) in { f =>
               val afOut = AudioFile.openWrite( f, fileSpec )
               assert( afOut.isOpen     )
               assert( afOut.isReadable )
               assert( afOut.isWritable )
               assert( afOut.position        === 0L )
               val buf = afOut.buffer( bufSize )
               afOut.write( buf )
               afOut.write( buf, 0, totalSize - bufSize )
               assert( afOut.position        === totalSize.toLong )
               val framesWritten = afOut.numFrames
               assert( framesWritten         === afOut.spec.numFrames )
               afOut.close()
               assert( !afOut.isOpen )

               val afIn = AudioFile.openRead( f )
               assert( afIn.isOpen     )
               assert( afIn.isReadable )
               assert( !afIn.isWritable )
               assert( afIn.position         === 0L )
               assert( afIn.numFrames        === framesWritten )
               assert( afIn.spec.numFrames   === framesWritten )
               assert( afIn.numChannels      === numCh )
               assert( afIn.sampleFormat     === smpFmt )
               assert( afIn.sampleRate       === sr )
               assert( afIn.file             === Some( f ))
               assert( afIn.fileType         === typ )
               afIn.read( buf )
               afIn.read( buf, 0, totalSize - bufSize )
               assert( afIn.position         === totalSize.toLong )
               afIn.close()
               assert( !afIn.isOpen )
            }
         }
      }
   }
}

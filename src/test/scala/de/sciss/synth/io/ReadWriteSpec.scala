package de.sciss.synth.io

/*
  To run only this test:

  test-only de.sciss.synth.io.ReadWriteSpec

 */
class ReadWriteSpec extends TempFileSpec {
  val rwTypes = AudioFileType.readable.collect {
    case cw: AudioFileType.CanWrite => cw
  }

   val chanNums = List( 1, 2, 3 )

   val bufSize    = 8192
   val totalSize  = 10000
   val size2      = totalSize - bufSize
   val sr         = 44100.0

   def generate( buf: Frames, num: Int, seed: Long = 0L ): Unit = {
      val rnd = new util.Random( seed )
      var ch = 0; while( ch < buf.length ) {
         val cb = buf( ch )
         var i = 0; while( i < num ) {
            cb( i ) = rnd.nextFloat()
         i += 1 }
      ch += 1 }
   }

   def compare( a: Frames, b: Frames, num: Int, fmt: SampleFormat ): Unit = {
      val tol = fmt match {
         case SampleFormat.Float    => 0.0
         case SampleFormat.Double   => 0.0
         case _ => 2.02 / (1L << fmt.bitsPerSample)   // whatever... ?
      }
      assert( a.length === b.length )
      var ch = 0; while( ch < a.length ) {
         val ca = a( ch )
         val cb = b( ch )
         var i = 0; while( i < num ) {
            val diff = math.abs( ca( i ) - cb( i ))
            assert( diff <= tol, "Buffer content differs (err = " + diff + ") for " + fmt )
         i += 1 }
      ch += 1 }
   }

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
               val bufOut = afOut.buffer( bufSize )
               generate( bufOut, bufSize, 0L )
               afOut.write( bufOut )
               generate( bufOut, size2, 1L )
               afOut.write( bufOut, 0, size2 )
               assert( afOut.position        === totalSize.toLong )
               val framesWritten = afOut.numFrames
               assert( framesWritten         === afOut.spec.numFrames )
               afOut.close()
               assert( !afOut.isOpen )

               val afIn = AudioFile.openRead( f )
               val bufIn = afIn.buffer( bufSize )
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
               afIn.read( bufIn )
               generate( bufOut, bufSize, 0L )
               compare( bufIn, bufOut, bufSize, smpFmt )
               afIn.read( bufIn, 0, size2 )
               generate( bufOut, size2, 1L )
               compare( bufIn, bufOut, size2, smpFmt )
               assert( afIn.position         === totalSize.toLong )
               afIn.close()
               assert( !afIn.isOpen )
            }
         }
      }
   }
}

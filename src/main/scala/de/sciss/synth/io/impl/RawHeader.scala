package de.sciss.synth.io
package impl

import java.io.{RandomAccessFile, DataOutputStream}
import java.nio.ByteOrder

private[io] object RawHeader {
   def write( raf: RandomAccessFile, spec: AudioFileSpec ) : WritableAudioFileHeader = initWrite( spec )
   def write( dos: DataOutputStream, spec: AudioFileSpec ) : WritableAudioFileHeader = initWrite( spec )

   private def initWrite( spec: AudioFileSpec ) : WritableAudioFileHeader = {
      val spec1 = if( spec.byteOrder.isDefined ) spec else spec.copy( byteOrder = Some( ByteOrder.nativeOrder() ))
      new NonUpdatingWritableHeader( spec1 )
   }
}
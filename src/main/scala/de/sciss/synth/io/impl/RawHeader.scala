package de.sciss.synth.io
package impl

import java.io.{RandomAccessFile, DataOutputStream}

private[io] object RawHeader {
   def write( dos: DataOutputStream, spec: AudioFileSpec) : WritableAudioFileHeader = {
      sys.error( "TODO" )
   }
   def write( raf: RandomAccessFile, spec: AudioFileSpec) : WritableAudioFileHeader = {
      sys.error( "TODO" )
   }
}

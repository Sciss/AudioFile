package de.sciss.synth.io
package impl

import java.io.IOException

private[io] final class NonUpdatingWritableHeader( val spec: AudioFileSpec )
extends WritableAudioFileHeader {
   def byteOrder  = spec.byteOrder.get

   @throws(classOf[ IOException ])
   def update( numFrames: Long ) {}
}

package de.sciss.synth.io
package impl

import java.io.IOException

private[io] final class NonUpdatingWritableHeader( spec0: AudioFileSpec )
extends WritableAudioFileHeader {
   private var numFrames0 = spec0.numFrames

   @throws( classOf[ IOException ])
   def update( numFrames: Long ) {
      numFrames0 = numFrames
   }

   def spec       = spec0.copy( numFrames = numFrames0 )
   def byteOrder  = spec0.byteOrder.get
}

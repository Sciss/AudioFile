/*
 *  NonUpdatingWritableHeader.java
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.io
package impl

import java.io.IOException
import java.nio.ByteOrder

private[io] final class NonUpdatingWritableHeader(val spec: AudioFileSpec)
  extends WritableAudioFileHeader {

  def byteOrder: ByteOrder = spec.byteOrder.get

  @throws(classOf[IOException])
  def update(numFrames: Long): Unit = ()
}

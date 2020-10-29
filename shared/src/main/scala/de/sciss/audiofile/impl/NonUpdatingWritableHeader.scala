/*
 *  NonUpdatingWritableHeader.java
 *  (AudioFile)
 *
 *  Copyright (c) 2004-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.audiofile.impl

import java.io.IOException
import java.nio.ByteOrder

import de.sciss.audiofile.{AudioFileSpec, WritableAudioFileHeader}

private[audiofile] final class NonUpdatingWritableHeader(val spec: AudioFileSpec)
  extends WritableAudioFileHeader {

  def byteOrder: ByteOrder = spec.byteOrder.get

  @throws(classOf[IOException])
  def update(numFrames: Long): Unit = ()
}

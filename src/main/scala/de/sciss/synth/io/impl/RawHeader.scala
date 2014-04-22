/*
 *  RawHeader.java
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.io
package impl

import java.io.{RandomAccessFile, DataOutputStream}
import java.nio.ByteOrder

private[io] object RawHeader {
  def write(raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = initWrite(spec)
  def write(dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader = initWrite(spec)

  private def initWrite(spec: AudioFileSpec): WritableAudioFileHeader = {
    val spec1 = if (spec.byteOrder.isDefined) spec else spec.copy(byteOrder = Some(ByteOrder.nativeOrder()))
    new NonUpdatingWritableHeader(spec1)
  }
}
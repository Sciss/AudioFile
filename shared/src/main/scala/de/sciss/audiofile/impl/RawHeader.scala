/*
 *  RawHeader.java
 *  (AudioFile)
 *
 *  Copyright (c) 2004-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.audiofile.impl

import java.io.{DataOutputStream, RandomAccessFile}
import java.nio.ByteOrder

import de.sciss.audiofile.{AudioFileSpec, WritableAudioFileHeader}

private[audiofile] object RawHeader {
  def write(raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = initWrite(spec)
  def write(dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader = initWrite(spec)

  private def initWrite(spec: AudioFileSpec): WritableAudioFileHeader = {
    val spec1 = if (spec.byteOrder.isDefined) spec else spec.copy(byteOrder = Some(ByteOrder.nativeOrder()))
    new NonUpdatingWritableHeader(spec1)
  }
}
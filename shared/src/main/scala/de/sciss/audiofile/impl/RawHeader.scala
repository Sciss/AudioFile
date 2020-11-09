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

import de.sciss.asyncfile.AsyncWritableByteChannel
import de.sciss.audiofile.{AsyncWritableAudioFileHeader, AudioFileSpec, WritableAudioFileHeader}

import scala.concurrent.Future

private[audiofile] object RawHeader {
  def write(raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = initWrite(spec)
  def write(dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader = initWrite(spec)

  def writeAsync(ch: AsyncWritableByteChannel, spec: AudioFileSpec): Future[AsyncWritableAudioFileHeader] =
    Future.successful(initWrite(spec))

  private def initWrite(spec: AudioFileSpec): WritableAudioFileHeader with AsyncWritableAudioFileHeader = {
    val spec1 = if (spec.byteOrder.isDefined) spec else spec.copy(byteOrder = Some(ByteOrder.nativeOrder()))
    new NonUpdatingWritableHeader(spec1)
  }
}
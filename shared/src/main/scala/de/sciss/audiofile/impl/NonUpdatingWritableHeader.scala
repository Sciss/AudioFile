/*
 *  NonUpdatingWritableHeader.java
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

import java.io.IOException
import java.nio.ByteOrder

import de.sciss.audiofile.{AsyncWritableAudioFileHeader, AudioFileSpec, WritableAudioFileHeader}

import scala.concurrent.Future

private[audiofile] final class NonUpdatingWritableHeader(val spec: AudioFileSpec)
  extends WritableAudioFileHeader with AsyncWritableAudioFileHeader {

  def byteOrder: ByteOrder = spec.byteOrder.get

  @throws(classOf[IOException])
  def update(numFrames: Long): Unit = ()

  def updateAsync(numFrames: Long): Future[Unit] = Future.unit
}

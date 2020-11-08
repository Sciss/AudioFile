/*
 *  BasicHeader.java
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

import java.io.{DataInput, DataInputStream, DataOutputStream, IOException, RandomAccessFile}

import de.sciss.audiofile.{AudioFileHeader, AudioFileSpec, WritableAudioFileHeader}

private[audiofile] trait BasicHeader {
  @throws(classOf[IOException])
  def identify(dis: DataInputStream): Boolean

  @throws(classOf[IOException])
  final def read(raf: RandomAccessFile): AudioFileHeader = readDataInput(raf)

  @throws(classOf[IOException])
  final def read(dis: DataInputStream): AudioFileHeader = readDataInput(dis)

  @throws(classOf[IOException])
  protected def readDataInput(din: DataInput): AudioFileHeader

  @throws(classOf[IOException])
  def write(raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader

  @throws(classOf[IOException])
  def write(dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader
}
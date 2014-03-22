/*
 *  BasicHeader.java
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.io
package impl

import java.io.{DataOutputStream, DataInput, DataInputStream, RandomAccessFile, IOException}

private[io] trait BasicHeader {
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
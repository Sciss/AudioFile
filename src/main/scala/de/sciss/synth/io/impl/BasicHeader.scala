/*
 *  BasicHeader.java
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	 For further information, please contact Hanns Holger Rutz at
 *	 contact@sciss.de
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
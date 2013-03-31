/*
 *  AudioFileSpec.scala
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

import java.nio.ByteOrder
import de.sciss.lucre.io.ImmutableSerializer
import de.sciss.lucre.io.DataOutput
import de.sciss.lucre.io.DataInput
import annotation.switch

object AudioFileSpec {
  implicit object Serializer extends ImmutableSerializer[AudioFileSpec] {
    def write(spec: AudioFileSpec, out: DataOutput) {
      val fid = spec.fileType match {
           case AudioFileType.AIFF    => 0
           case AudioFileType.Wave    => 1
           case AudioFileType.Wave64  => 2
           case AudioFileType.IRCAM   => 3
           case AudioFileType.NeXT    => 4
           case other                 => sys.error( "Unexpected audio file type " + other )
        }
        out.writeByte( fid )
        val sid = spec.sampleFormat match {
           case SampleFormat.Int16    => 0
           case SampleFormat.Int24    => 1
           case SampleFormat.Float    => 2
           case SampleFormat.Int32    => 3
           case SampleFormat.Double   => 4
           case SampleFormat.UInt8    => 5
           case SampleFormat.Int8     => 6
           case other                 => sys.error( "Unexpected sample format " + other )
        }
        out.writeByte( sid )
        out.writeInt( spec.numChannels )
        out.writeDouble( spec.sampleRate )
        val bid = spec.byteOrder match {
           case None                              => 0
           case Some( ByteOrder.LITTLE_ENDIAN )   => 1
           case Some( ByteOrder.BIG_ENDIAN )      => 2
           case other                             => sys.error( "Unexpected byte order " + other )
        }
       out.writeByte(bid)
       out.writeLong(spec.numFrames)
     }

    def read(in: DataInput): AudioFileSpec = {
        val fileType = (in.readByte(): @switch) match {
           case 0   => AudioFileType.AIFF
           case 1   => AudioFileType.Wave
           case 2   => AudioFileType.Wave64
           case 3   => AudioFileType.IRCAM
           case 4   => AudioFileType.NeXT
           case other => sys.error( "Unexpected audio file type ID " + other )
        }
        val sampleFormat = (in.readByte(): @switch) match {
           case 0   => SampleFormat.Int16
           case 1   => SampleFormat.Int24
           case 2   => SampleFormat.Float
           case 3   => SampleFormat.Int32
           case 4   => SampleFormat.Double
           case 5   => SampleFormat.UInt8
           case 6   => SampleFormat.Int8
           case other => sys.error( "Unexpected sample format ID " + other )
        }
        val numChannels   = in.readInt()
        val sampleRate    = in.readDouble()
        val byteOrder     = (in.readByte(): @switch) match {
           case 0   => None
           case 1   => Some( ByteOrder.LITTLE_ENDIAN )
           case 2   => Some( ByteOrder.BIG_ENDIAN )
           case other => sys.error( "Unexpected byte order ID " + other )
        }
        val numFrames     = in.readLong()
      AudioFileSpec(fileType, sampleFormat, numChannels = numChannels, sampleRate = sampleRate,
        byteOrder = byteOrder, numFrames = numFrames)
     }
  }
}
final case class AudioFileSpec(fileType: AudioFileType = AudioFileType.AIFF,
                               sampleFormat: SampleFormat = SampleFormat.Float,
                               numChannels: Int,
                               sampleRate: Double,
                               byteOrder: Option[ByteOrder] = None,
                               numFrames: Long = 0L)

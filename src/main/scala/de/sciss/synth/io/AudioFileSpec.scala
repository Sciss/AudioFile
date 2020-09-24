/*
 *  AudioFileSpec.scala
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

package de.sciss.synth.io

import java.nio.ByteOrder

import de.sciss.serial.{DataInput, DataOutput, ConstFormat}

import scala.annotation.switch

object AudioFileSpec {
  implicit object format extends ConstFormat[AudioFileSpec] {
    def write(spec: AudioFileSpec, out: DataOutput): Unit = {
      val fid = spec.fileType match {
        case AudioFileType.AIFF    => 0
        case AudioFileType.Wave    => 1
        case AudioFileType.Wave64  => 2
        case AudioFileType.IRCAM   => 3
        case AudioFileType.NeXT    => 4
        case other                 => sys.error(s"Unexpected audio file type $other")
      }
      out.writeByte(fid)
      val sid = spec.sampleFormat match {
        case SampleFormat.Int16    => 0
        case SampleFormat.Int24    => 1
        case SampleFormat.Float    => 2
        case SampleFormat.Int32    => 3
        case SampleFormat.Double   => 4
        case SampleFormat.UInt8    => 5
        case SampleFormat.Int8     => 6
        case other                 => sys.error(s"Unexpected sample format $other")
      }
      out.writeByte(sid)
      out.writeInt(spec.numChannels)
      out.writeDouble(spec.sampleRate)
      val bid = spec.byteOrder match {
        case None                            => 0
        case Some(ByteOrder.LITTLE_ENDIAN)   => 1
        case Some(ByteOrder.BIG_ENDIAN)      => 2
        case other                           => sys.error(s"Unexpected byte order $other")
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
        case other => sys.error(s"Unexpected audio file type ID $other")
      }
      val sampleFormat = (in.readByte(): @switch) match {
        case 0   => SampleFormat.Int16
        case 1   => SampleFormat.Int24
        case 2   => SampleFormat.Float
        case 3   => SampleFormat.Int32
        case 4   => SampleFormat.Double
        case 5   => SampleFormat.UInt8
        case 6   => SampleFormat.Int8
        case other => sys.error(s"Unexpected sample format ID $other")
      }
      val numChannels = in.readInt()
      val sampleRate  = in.readDouble()
      val byteOrder   = (in.readByte(): @switch) match {
        case 0   => None
        case 1   => Some( ByteOrder.LITTLE_ENDIAN )
        case 2   => Some( ByteOrder.BIG_ENDIAN )
        case other => sys.error( "Unexpected byte order ID " + other )
      }
      val numFrames = in.readLong()
      AudioFileSpec(fileType = fileType, sampleFormat = sampleFormat, numChannels = numChannels,
        sampleRate = sampleRate, byteOrder = byteOrder, numFrames = numFrames)
    }
  }
}
final case class AudioFileSpec(fileType     : AudioFileType     = AudioFileType.AIFF,
                               sampleFormat : SampleFormat      = SampleFormat.Float,
                               numChannels  : Int,
                               sampleRate   : Double,
                               byteOrder    : Option[ByteOrder] = None,
                               numFrames    : Long              = 0L
)

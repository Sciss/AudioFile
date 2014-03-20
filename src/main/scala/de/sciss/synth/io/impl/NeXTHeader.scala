/*
 *  NeXTHeader.scala
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.io
package impl

import java.io.{DataOutputStream, DataOutput, DataInput, DataInputStream, RandomAccessFile, IOException}
import annotation.switch
import java.nio.ByteOrder

/** NeXT or SND format.
  *
  * Info: http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/AU/AU.html
  */
private[io] object NeXTHeader {
  private final val SND_MAGIC = 0x2E736E64 // '.snd'

  @throws(classOf[IOException])
  def identify(dis: DataInputStream) = dis.readInt() == SND_MAGIC

  import AudioFileHeader._

  @throws(classOf[IOException])
  def read(raf: RandomAccessFile): AudioFileHeader = readDataInput(raf, raf.length())

  @throws(classOf[IOException])
  def read(dis: DataInputStream ): AudioFileHeader = readDataInput(dis, dis.available())

  @throws(classOf[IOException])
  private def readDataInput(din: DataInput, fileLen: Long): AudioFileHeader = {
    if (din.readInt() != SND_MAGIC) formatError()

    val dataOffset    = din.readInt() // offset in bytes
    val dataSize_?    = din.readInt()
    val sampleFormat  = (din.readInt(): @switch) match {
      case 2 => SampleFormat.Int8   // 8 bit linear
      case 3 => SampleFormat.Int16  // 16 bit linear
      case 4 => SampleFormat.Int24  // 24 bit linear
      case 5 => SampleFormat.Int32  // 32 bit linear
      case 6 => SampleFormat.Float  // 32 bit float
      case 7 => SampleFormat.Double // 64 bit float
      case m => throw new IOException(s"Unsupported NeXT encoding ($m)")
    }
    val sampleRate  = din.readInt().toDouble
    val numChannels = din.readInt()

    val skp = dataOffset - 24 // current pos is 24
    if (skp > 0) din.skipBytes(skp)
    val frameSize = ((sampleFormat.bitsPerSample + 7) >> 3) * numChannels

    val dataSize  = if (dataSize_? == 0xFFFFFFFF) fileLen - dataOffset else dataSize_?
    val numFrames = math.max(0L, dataSize) / frameSize

    val spec = new AudioFileSpec(fileType = AudioFileType.NeXT, sampleFormat = sampleFormat,
      numChannels = numChannels, sampleRate = sampleRate, byteOrder = Some(ByteOrder.BIG_ENDIAN),
      numFrames = numFrames)
    ReadableAudioFileHeader(spec, ByteOrder.BIG_ENDIAN)
  }

  @throws(classOf[IOException])
  def write(raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = {
    val spec1 = writeDataOutput(raf, spec, writeSize = false)
    new WritableFileHeader(raf, spec1)
  }

  @throws(classOf[IOException])
  def write(dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader = {
    val spec1 = writeDataOutput(dos, spec, writeSize = true)
    new NonUpdatingWritableHeader(spec1)
  }

  @throws(classOf[IOException])
  private def writeDataOutput(dout: DataOutput, spec: AudioFileSpec, writeSize: Boolean): AudioFileSpec = {
    val res = spec.byteOrder match {
      case Some(ByteOrder.BIG_ENDIAN) => spec
      case None                       => spec.copy(byteOrder = Some(ByteOrder.BIG_ENDIAN))
      case Some(other)                => throw new IOException(s"Unsupported byte order $other")
    }

    //      val str = (String) descr.getProperty( AudioFileInfo.KEY_COMMENT );
    val dataOffset = 28L // if( str == null ) 28L else ((28 + str.length()) & ~3).toLong
    dout.writeInt(SND_MAGIC)
    dout.writeInt(dataOffset.toInt)
    val dataSize = if (writeSize) spec.numFrames * ((spec.sampleFormat.bitsPerSample >> 3) * spec.numChannels) else 0L
    dout.writeInt(dataSize.toInt)

    val formatCode = spec.sampleFormat match {
      case SampleFormat.Int8    => 2
      case SampleFormat.Int16   => 3
      case SampleFormat.Int24   => 4
      case SampleFormat.Int32   => 5
      case SampleFormat.Float   => 6
      case SampleFormat.Double  => 7
    }
    dout.writeInt(formatCode)
    dout.writeInt((spec.sampleRate + 0.5).toInt)
    dout.writeInt(spec.numChannels)

    // comment
    //      if( str == null ) {
    dout.writeInt(0) // minimum 4 byte character data
    //      } else {
    //         ...
    //      }

    res
  }

  final private class WritableFileHeader(raf: RandomAccessFile, val spec: AudioFileSpec)
    extends WritableAudioFileHeader {

    private var numFrames0 = 0L

    @throws(classOf[IOException])
    def update(numFrames: Long): Unit = {
      if (numFrames == numFrames0) return

      val dataSize  = numFrames * ((spec.sampleFormat.bitsPerSample >> 3) * spec.numChannels)
      val oldPos    = raf.getFilePointer
      raf.seek(8L)
      raf.writeInt(dataSize.toInt)
      raf.seek(oldPos)
      numFrames0 = numFrames
    }

    def byteOrder: ByteOrder = spec.byteOrder.get
  }
}
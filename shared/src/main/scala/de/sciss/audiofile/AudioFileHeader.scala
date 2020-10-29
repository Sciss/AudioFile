/*
 *  AudioFileHeader.java
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

package de.sciss.audiofile

import java.io.{DataInput, DataInputStream, DataOutput, DataOutputStream, IOException, RandomAccessFile}
import java.nio.ByteOrder

import scala.concurrent.Future

private[audiofile] object AudioFileHeader {
  def opNotSupported = throw new IOException("Operation not supported")

  @throws(classOf[IOException])
  @inline def readLittleUShort(din: DataInput): Int = {
    val i = din.readUnsignedShort()
    (i >> 8) | ((i & 0xFF) << 8)
  }

  @throws(classOf[IOException])
  @inline def readLittleInt(din: DataInput): Int = {
    val i = din.readInt()
    ((i >> 24) & 0xFF) | ((i >> 8) & 0xFF00) | ((i << 8) & 0xFF0000) | (i << 24)
  }

  @throws(classOf[IOException])
  @inline def readLittleFloat(din: DataInput): Float = {
    val i = din.readInt()
    java.lang.Float.intBitsToFloat(((i >> 24) & 0xFF) | ((i >> 8) & 0xFF00) | ((i << 8) & 0xFF0000) | (i << 24))
  }

  @throws(classOf[IOException])
  @inline def readLittleLong(din: DataInput): Long = {
    val n = din.readLong()
    ((n >> 56) & 0xFFL) |
    ((n >> 40) & 0xFF00L) |
    ((n >> 24) & 0xFF0000L) |
    ((n >>  8) & 0xFF000000L) |
    ((n <<  8) & 0xFF00000000L) |
    ((n << 24) & 0xFF0000000000L) |
    ((n << 40) & 0xFF000000000000L) |
     (n << 56)
  }

  @throws(classOf[IOException])
  @inline def writeLittleShort(dout: DataOutput, i: Int): Unit =
    dout.writeShort((i >> 8) | ((i & 0xFF) << 8))

  @throws(classOf[IOException])
  @inline def writeLittleInt(dout: DataOutput, i: Int): Unit =
    dout.writeInt(((i >> 24) & 0xFF) | ((i >> 8) & 0xFF00) | ((i << 8) & 0xFF0000) | (i << 24))

  @throws(classOf[IOException])
  @inline def writeLittleFloat(dout: DataOutput, f: Float): Unit = {
    val i = java.lang.Float.floatToIntBits(f)
    dout.writeInt(((i >> 24) & 0xFF) | ((i >> 8) & 0xFF00) | ((i << 8) & 0xFF0000) | (i << 24))
  }

  @throws(classOf[IOException])
  @inline def writeLittleLong(dout: DataOutput, n: Long): Unit = {
    dout.writeLong(
      ((n >> 56) & 0xFFL) |
      ((n >> 40) & 0xFF00L) |
      ((n >> 24) & 0xFF0000L) |
      ((n >>  8) & 0xFF000000L) |
      ((n <<  8) & 0xFF00000000L) |
      ((n << 24) & 0xFF0000000000L) |
      ((n << 40) & 0xFF000000000000L) |
       (n << 56))
  }

  @throws(classOf[IOException])
  def readNullTermString(din: DataInput): String = {
    val buf = new StringBuffer()
    var b   = din.readByte()
    while (b != 0) {
      buf.append(b.toChar)
      b = din.readByte()
    }
    buf.toString
  }

  def formatError     () = throw new IOException("A header format error occurred")
  def encodingError   () = throw new IOException("File has unsupported encoding")
  def incompleteError () = throw new IOException("Header data is incomplete")

  trait DataInputReader {
    @throws(classOf[IOException]) def readInt  (): Int
    @throws(classOf[IOException]) def readFloat(): Float

    def byteOrder: ByteOrder
  }

  trait DataOutputWriter {
    @throws(classOf[IOException]) def writeInt  (i: Int  ): Unit
    @throws(classOf[IOException]) def writeFloat(f: Float): Unit

    def byteOrder: ByteOrder
  }

  def dataInputReader(din: DataInput, byteOrder: ByteOrder): DataInputReader =
    if (byteOrder == ByteOrder.LITTLE_ENDIAN) {
      new LittleDataInputReader(din)
    } else {
      new BigDataInputReader(din)
    }

  def nativeDataInputReader(din: DataInput): DataInputReader = dataInputReader(din, ByteOrder.nativeOrder)

  def dataOutputWriter(dout: DataOutput, byteOrder: ByteOrder): DataOutputWriter =
    if (byteOrder == ByteOrder.LITTLE_ENDIAN) {
      new LittleDataOutputWriter(dout)
    } else {
      new BigDataOutputWriter(dout)
    }

  def nativeDataOutputWriter(dout: DataOutput): DataOutputWriter = dataOutputWriter(dout, ByteOrder.nativeOrder)

  final class BigDataInputReader(din: DataInput) extends DataInputReader {
    @throws(classOf[IOException]) def readInt  (): Int    = din.readInt()
    @throws(classOf[IOException]) def readFloat(): Float  = din.readFloat()

    def byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  }

  final class LittleDataInputReader(din: DataInput) extends DataInputReader {
    @throws(classOf[IOException]) def readInt  (): Int    = readLittleInt(din)
    @throws(classOf[IOException]) def readFloat(): Float  = readLittleFloat(din)

    def byteOrder: ByteOrder = ByteOrder.LITTLE_ENDIAN
  }

  final class BigDataOutputWriter(dout: DataOutput) extends DataOutputWriter {
    @throws(classOf[IOException]) def writeInt(i: Int): Unit =
      dout.writeInt(i)

    @throws(classOf[IOException]) def writeFloat(f: Float): Unit =
      dout.writeFloat(f)

    def byteOrder: ByteOrder = ByteOrder.BIG_ENDIAN
  }

  final class LittleDataOutputWriter(dout: DataOutput) extends DataOutputWriter {
    @throws(classOf[IOException]) def writeInt(i: Int): Unit =
      writeLittleInt(dout, i)

    @throws(classOf[IOException]) def writeFloat(f: Float): Unit =
      writeLittleFloat(dout, f)

    def byteOrder: ByteOrder = ByteOrder.LITTLE_ENDIAN
  }
}

private[audiofile] trait AudioFileHeader {
  def byteOrder: ByteOrder

  def spec: AudioFileSpec
}

private[audiofile] case class ReadableAudioFileHeader(spec: AudioFileSpec, byteOrder: ByteOrder)
  extends AudioFileHeader

private[audiofile] trait WritableAudioFileHeader extends AudioFileHeader {
  @throws(classOf[IOException])
  def update(numFrames: Long): Unit
}

private[audiofile] trait AsyncWritableAudioFileHeader extends AudioFileHeader {
  def update(numFrames: Long): Future[Unit]
}

private[audiofile] trait AudioFileHeaderReader {
  /** Reads in the header information. Seek position
    * should remain at the first frame of the audio data.
    * If the header does not support a plain input stream
    * but requires random access or length information,
    * it should throw an IOException
    */
  @throws(classOf[IOException])
  def read(dis: DataInputStream): AudioFileHeader

  @throws(classOf[IOException])
  def read(raf: RandomAccessFile): AudioFileHeader
}

private[audiofile] trait AudioFileHeaderWriter {
  @throws(classOf[IOException])
  def write(dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader

  @throws(classOf[IOException])
  def write(raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader
}
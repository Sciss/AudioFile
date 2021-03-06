/*
 *  BufferHandler.scala
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

package de.sciss.audiofile

import java.io.IOException
import java.nio.channels.{ReadableByteChannel, WritableByteChannel}
import java.nio.{Buffer, ByteBuffer, ByteOrder}

import AudioFile.Frames

import scala.{Byte => SByte, Double => SDouble, Float => SFloat, Int => SInt, Short => SShort}

private[audiofile] trait BufferHandler {
  protected def byteBuf: ByteBuffer

  def numChannels: SInt

  protected def bitsPerSample: SInt

  final val frameSize: SInt = (bitsPerSample >> 3) * numChannels  // bytes per frame

  protected final val bufFrames = byteBuf.capacity / frameSize

  if (bufFrames <= 0) throw new IllegalArgumentException("Buffer too small")
}

private[audiofile] trait BufferReader extends BufferHandler {
  @throws(classOf[IOException])
  def read(frames: Frames, off: SInt, len: SInt): Unit

  protected def reader: ReadableByteChannel
}

private[audiofile] trait BufferWriter extends BufferHandler {
  @throws(classOf[IOException])
  def write(frames: Frames, off: SInt, len: SInt): Unit

  protected def writer: WritableByteChannel
}

private[audiofile] trait BufferBidi extends BufferReader with BufferWriter

// -------------- Basic --------------

private[audiofile] object BufferHandler {

  abstract class Byte extends BufferHandler {
    protected final def bitsPerSample = 8
    protected final val arrayBuf      = new Array[SByte](byteBuf.capacity())
  }

  abstract class UByte extends BufferHandler {
    protected final def bitsPerSample = 8
    protected final val arrayBuf      = new Array[SByte](byteBuf.capacity())
  }

  abstract class Short extends BufferHandler {
    protected final def bitsPerSample = 16

    (byteBuf: Buffer).clear()
    protected final val viewBuf   = byteBuf.asShortBuffer()
    protected final val arrayBuf  = new Array[SShort](viewBuf.capacity())
  }

  abstract class ThreeBytes extends BufferHandler {
    protected final def bitsPerSample = 24

    // note : it's *not* faster to use ByteBuffer.allocate()
    // and ByteBuffer.array() than this implementation
    // (using ByteBuffer.allocateDirect() and bulk get into a separate arrayBuf)
    protected final val arrayBuf  = new Array[SByte](byteBuf.capacity())
//    protected final val chStep    = numChannels * 3
  }

  abstract class Int extends BufferHandler {
    protected final def bitsPerSample = 32

    (byteBuf: Buffer).clear()
    protected final val viewBuf   = byteBuf.asIntBuffer()
    protected final val arrayBuf  = new Array[SInt](viewBuf.capacity())
  }

  abstract class Float extends BufferHandler {
    protected final def bitsPerSample = 32

    (byteBuf: Buffer).clear()
    protected final val viewBuf   = byteBuf.asFloatBuffer()
    protected final val arrayBuf  = new Array[SFloat](viewBuf.capacity())
  }

  abstract class Double extends BufferHandler {
    protected final def bitsPerSample = 64

    (byteBuf: Buffer).clear()
    protected final val viewBuf   = byteBuf.asDoubleBuffer()
    protected final val arrayBuf  = new Array[SDouble](viewBuf.capacity())
  }

}

// -------------- Reader --------------

private[audiofile] trait BufferReaderFactory {
  def apply(read: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt): BufferReader
}

private[audiofile] object BufferReader {

  object Byte extends BufferReaderFactory

  final case class Byte(reader: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Byte with ByteLike

  // float to byte = f*0x7F+0x80 (-1 ... +1becomes 0x01 to 0xFF)
  // which is how libsndfile behaves
  object UByte extends BufferReaderFactory

  final case class UByte(reader: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.UByte with UByteLike

  object Short extends BufferReaderFactory

  final case class Short(reader: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Short with ShortLike

  object ThreeBytes extends BufferReaderFactory {
    def apply(read: ReadableByteChannel, byteBuf: ByteBuffer,
              numChannels: SInt): BufferReader = {
      if (byteBuf.order() == ByteOrder.LITTLE_ENDIAN) {
        ThreeBytesLE(read, byteBuf, numChannels)
      } else {
        ThreeBytesBE(read, byteBuf, numChannels)
      }
    }
  }

  /*
   *  24bit big endian
   */
  final case class ThreeBytesBE(reader: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.ThreeBytes with ThreeBytesBELike

  /*
   *  24bit little endian
   */
  final case class ThreeBytesLE(reader: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.ThreeBytes with ThreeBytesLELike

  object Int extends BufferReaderFactory

  final case class Int(reader: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Int with IntLike

  object Float extends BufferReaderFactory

  final case class Float(reader: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Float with FloatLike

  object Double extends BufferReaderFactory

  final case class Double(reader: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Double with DoubleLike

  trait ByteLike extends BufferReader {
    me: BufferHandler.Byte =>

    final def read(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position   = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * frameSize
        (byteBuf: Buffer).rewind().limit(m)
        reader.read(byteBuf)
        (byteBuf: Buffer).flip()
        byteBuf.get(arrayBuf, 0, m)
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              b(j) = arrayBuf(i).toDouble / 0x7F
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait UByteLike extends BufferReader {
    me: BufferHandler.UByte =>

    final def read(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m         = chunkLen * frameSize
        (byteBuf: Buffer).rewind().limit(m)
        reader.read(byteBuf)
        (byteBuf: Buffer).flip()
        byteBuf.get(arrayBuf, 0, m)
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              val arr1 = arrayBuf(i)
              // java can't handle unsigned bytes
              b(j) = (if (arr1 < 0) 0x80 + arr1 else arr1 - 0x80).toDouble / 0x7F
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait ShortLike extends BufferReader {
    me: BufferHandler.Short =>

    final def read(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * numChannels
        (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
        reader.read(byteBuf)
        (viewBuf: Buffer).clear()
        viewBuf.get(arrayBuf, 0, m)
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              b(j) = arrayBuf(i).toDouble / 0x7FFF
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait ThreeBytesBELike extends BufferReader {
    me: BufferHandler.ThreeBytes =>

    final def read(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * frameSize
        (byteBuf: Buffer).rewind().limit(m)
        reader.read(byteBuf)
        (byteBuf: Buffer).flip()
        byteBuf.get(arrayBuf, 0, m)
        var ch = 0; val iStep = numChannels * 3; var p = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = p; var j = position; while (i < m) {
              b(j) = ((arrayBuf(i) << 16 ) |
                     ((arrayBuf(i + 1) & 0xFF) << 8) |
                      (arrayBuf(i + 2) & 0xFF)).toDouble / 0x7FFFFF
              i += iStep; j += 1
            }
          }
          ch += 1; p += 3
        }
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait ThreeBytesLELike extends BufferReader {
    me: BufferHandler.ThreeBytes =>

    final def read(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * frameSize
        (byteBuf: Buffer).rewind().limit(m)
        reader.read(byteBuf)
        (byteBuf: Buffer).flip()
        byteBuf.get(arrayBuf, 0, m)
        var ch = 0; val iStep = numChannels * 3; var p = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = p; var j = position; while (i < m) {
              b(j) = ((arrayBuf(i) & 0xFF)|
                     ((arrayBuf(i + 1) & 0xFF) << 8) |
                      (arrayBuf(i + 2) << 16 )).toDouble / 0x7FFFFF
              i += iStep; j += 1
            }
          }
          ch += 1; p += 3
        }
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait IntLike extends BufferReader {
    me: BufferHandler.Int =>

    final def read(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen   = math.min(bufFrames, remaining)
        val m			   = chunkLen * numChannels
        (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
        reader.read(byteBuf)
        (viewBuf: Buffer).clear()
        viewBuf.get(arrayBuf, 0, m)
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              b(j) = arrayBuf(i).toDouble / 0x7FFFFFFF
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait FloatLike extends BufferReader {
    me: BufferHandler.Float =>

    final def read(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen   = math.min(bufFrames, remaining)
        val m          = chunkLen * numChannels
        (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
        reader.read(byteBuf)
        (viewBuf: Buffer).clear()
        viewBuf.get(arrayBuf, 0, m)
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              b(j) = arrayBuf(i)
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait DoubleLike extends BufferReader {
    me: BufferHandler.Double =>

    final def read(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen   = math.min(bufFrames, remaining)
        val m			   = chunkLen * numChannels
        (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
        reader.read(byteBuf)
        (viewBuf: Buffer).clear()
        viewBuf.get(arrayBuf, 0, m)
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              b(j) = arrayBuf(i)
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }
}

// -------------- Write --------------

private[audiofile] trait BufferWriterFactory {
  def apply(write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt): BufferWriter
}

private[audiofile] object BufferWriter {

  object Byte extends BufferWriterFactory

  final case class Byte(writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Byte with ByteLike

  // float to byte = f*0x7F+0x80 (-1 ... +1becomes 0x01 to 0xFF)
  // which is how libsndfile behaves
  object UByte extends BufferWriterFactory

  final case class UByte(writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.UByte with UByteLike {
    //      checkCapacity()
  }

  object Short extends BufferWriterFactory

  final case class Short(writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Short with ShortLike {
    //      checkCapacity()
  }

  object ThreeBytes extends BufferWriterFactory {
    def apply(write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt): BufferWriter =
      if (byteBuf.order() == ByteOrder.LITTLE_ENDIAN) {
        ThreeBytesLE(write, byteBuf, numChannels)
      } else {
        ThreeBytesBE(write, byteBuf, numChannels)
      }
  }

  final case class ThreeBytesBE(writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.ThreeBytes with ThreeBytesBELike

  final case class ThreeBytesLE(writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.ThreeBytes with ThreeBytesLELike

  object Int extends BufferWriterFactory

  final case class Int(writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Int with IntLike

  object Float extends BufferWriterFactory

  final case class Float(writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Float with FloatLike

  object Double extends BufferWriterFactory

  final case class Double(writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Double with DoubleLike

  trait ByteLike extends BufferWriter {
    me: BufferHandler.Byte =>

    final def write(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * frameSize
        var ch = 0; while(ch < numChannels) {
          val b = frames(ch)
          var i = ch; var j = position; while (i < m) {
            arrayBuf(i) = (b(j) * 0x7F).toByte
            i += numChannels; j += 1
          }
          ch += 1
        }
        (byteBuf: Buffer).clear()
        byteBuf.put(arrayBuf, 0, m)
        (byteBuf: Buffer).flip()
        writer.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait UByteLike extends BufferWriter {
    me: BufferHandler.UByte =>

    final def write(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * frameSize
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          var i = ch; var j = position; while (i < m) {
            arrayBuf(i) = (b(j) * 0x7F + 0x80).toByte
            i += numChannels; j += 1
          }
          ch += 1
        }
        (byteBuf: Buffer).clear()
        byteBuf.put(arrayBuf, 0, m)
        (byteBuf: Buffer).flip()
        writer.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait ShortLike extends BufferWriter {
    me: BufferHandler.Short =>

    final def write(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * numChannels
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              arrayBuf(i) = (b(j) * 0x7FFF).toShort
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        (viewBuf: Buffer).clear()
        viewBuf.put(arrayBuf, 0, m)
        (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
        writer.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait ThreeBytesBELike extends BufferWriter {
    me: BufferHandler.ThreeBytes =>

    final def write(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * frameSize
        var ch = 0; val iStep = numChannels * 3; var p = 0; while (ch < numChannels) {
          val b = frames(ch)
          var i = p; var j = position; while (i < m) {
            val k = (b(j) * 0x7FFFFF).toInt
            arrayBuf(i)     = (k >> 16).toByte
            arrayBuf(i + 1) = (k >> 8).toByte
            arrayBuf(i + 2) = k.toByte
            i += iStep; j += 1
          }
          ch += 1; p += 3
        }
        (byteBuf: Buffer).clear()
        byteBuf.put(arrayBuf, 0, m)
        (byteBuf: Buffer).flip()
        writer.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait ThreeBytesLELike extends BufferWriter {
    me: BufferHandler.ThreeBytes =>

    final def write(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * frameSize
        var ch = 0; val iStep = numChannels * 3; var p = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = p; var j = position; while (i < m) {
              val k = (b(j) * 0x7FFFFF).toInt
              arrayBuf(i)     = k.toByte
              arrayBuf(i + 1) = (k >> 8).toByte
              arrayBuf(i + 2) = (k >> 16).toByte
              i += iStep; j += 1
            }
          }
          ch += 1; p += 3
        }
        (byteBuf: Buffer).clear()
        byteBuf.put(arrayBuf, 0, m)
        (byteBuf: Buffer).flip()
        writer.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait IntLike extends BufferWriter {
    me: BufferHandler.Int =>

    final def write(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen   = math.min(bufFrames, remaining)
        val m	         = chunkLen * numChannels
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              arrayBuf(i) = (b(j) * 0x7FFFFFFF).toInt
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        (viewBuf: Buffer).clear()
        viewBuf.put(arrayBuf, 0, m)
        (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
        writer.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait FloatLike extends BufferWriter {
    me: BufferHandler.Float =>

    final def write(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * numChannels
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          var i = ch; var j = position; while (i < m) {
            arrayBuf(i) = b(j).toFloat
            i += numChannels; j += 1
          }
          ch += 1
        }
        (viewBuf: Buffer).clear()
        viewBuf.put(arrayBuf, 0, m)
        (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
        writer.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }

  trait DoubleLike extends BufferWriter {
    me: BufferHandler.Double =>

    final def write(frames: Frames, off: SInt, len: SInt): Unit = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * numChannels
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              arrayBuf(i) = b(j)
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        (viewBuf: Buffer).clear()
        viewBuf.put(arrayBuf, 0, m)
        (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
        writer.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
    }
  }
}

// -------------- Handler -------------- 

private[audiofile] trait BufferBidiFactory {
  def apply(read: ReadableByteChannel, write: WritableByteChannel, byteBuf: ByteBuffer,
            numChannels: SInt): BufferBidi
}

private[audiofile] object BufferBidi {

  object Byte extends BufferBidiFactory

  final case class Byte(reader: ReadableByteChannel, writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Byte with BufferReader.ByteLike with BufferWriter.ByteLike with BufferBidi

  object UByte extends BufferBidiFactory

  final case class UByte(reader: ReadableByteChannel, writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.UByte with BufferReader.UByteLike with BufferWriter.UByteLike with BufferBidi

  object Short extends BufferBidiFactory

  final case class Short(reader: ReadableByteChannel, writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Short with BufferReader.ShortLike with BufferWriter.ShortLike with BufferBidi

  object ThreeBytes extends BufferBidiFactory {
    def apply(read: ReadableByteChannel, write: WritableByteChannel, byteBuf: ByteBuffer,
              numChannels: SInt): BufferBidi =
      if (byteBuf.order() == ByteOrder.LITTLE_ENDIAN) {
        ThreeBytesLE(read, write, byteBuf, numChannels)
      } else {
        ThreeBytesBE(read, write, byteBuf, numChannels)
      }
  }

  final case class ThreeBytesBE(reader: ReadableByteChannel, writer: WritableByteChannel,
                                byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.ThreeBytes with BufferReader.ThreeBytesBELike with BufferWriter.ThreeBytesBELike
    with BufferBidi

  final case class ThreeBytesLE(reader: ReadableByteChannel, writer: WritableByteChannel,
                                byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.ThreeBytes with BufferReader.ThreeBytesLELike with BufferWriter.ThreeBytesLELike
    with BufferBidi

  object Int extends BufferBidiFactory

  final case class Int(reader: ReadableByteChannel, writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Int with BufferReader.IntLike with BufferWriter.IntLike with BufferBidi

  object Float extends BufferBidiFactory

  final case class Float(reader: ReadableByteChannel, writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Float with BufferReader.FloatLike with BufferWriter.FloatLike with BufferBidi

  object Double extends BufferBidiFactory

  final case class Double(reader: ReadableByteChannel, writer: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
    extends BufferHandler.Double with BufferReader.DoubleLike with BufferWriter.DoubleLike with BufferBidi
}
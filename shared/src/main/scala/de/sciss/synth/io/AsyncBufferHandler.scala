/*
 *  AsyncBufferHandler.scala
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

import java.io.IOException
import java.nio.{Buffer, ByteBuffer, ByteOrder}

import de.sciss.synth.io.AudioFile.Frames

import scala.concurrent.{ExecutionContext, Future}
import scala.{Int => SInt}
import scala.math.min

private[io] trait AsyncBufferHandler extends BufferHandler {
  implicit def executionContext: ExecutionContext
}

private[io] trait AsyncBufferReader extends AsyncBufferHandler {
  @throws(classOf[IOException])
  def read(frames: Frames, off: SInt, len: SInt): Future[Unit]

  val channel: AsyncReadableByteChannel
}

private[io] trait AsyncBufferWriter extends AsyncBufferHandler {
  @throws(classOf[IOException])
  def write(frames: Frames, off: SInt, len: SInt): Future[Unit]

  val channel: AsyncWritableByteChannel
}

private[io] trait AsyncBufferBidi extends AsyncBufferReader with AsyncBufferWriter

// -------------- Reader --------------

private[io] trait AsyncBufferReaderFactory {
  def apply(read: AsyncReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
           (implicit ec: ExecutionContext): AsyncBufferReader
}

private[io] object AsyncBufferReader {

  object Byte extends AsyncBufferReaderFactory

  final case class Byte(channel: AsyncReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                       (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Byte with ByteLike

  // float to byte = f*0x7F+0x80 (-1 ... +1becomes 0x01 to 0xFF)
  // which is how libsndfile behaves
  object UByte extends AsyncBufferReaderFactory

  final case class UByte(channel: AsyncReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                        (implicit val executionContext: ExecutionContext)
    extends BufferHandler.UByte with UByteLike

  object Short extends AsyncBufferReaderFactory

  final case class Short(channel: AsyncReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                        (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Short with ShortLike

  object ThreeBytes extends AsyncBufferReaderFactory {
    def apply(read: AsyncReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
             (implicit ec: ExecutionContext): AsyncBufferReader = {
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
  final case class ThreeBytesBE(channel: AsyncReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                               (implicit val executionContext: ExecutionContext)
    extends BufferHandler.ThreeBytes with ThreeBytesBELike

  /*
   *  24bit little endian
   */
  final case class ThreeBytesLE(channel: AsyncReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                               (implicit val executionContext: ExecutionContext)
    extends BufferHandler.ThreeBytes with ThreeBytesLELike

  object Int extends AsyncBufferReaderFactory

  final case class Int(channel: AsyncReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                      (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Int with IntLike

  object Float extends AsyncBufferReaderFactory

  final case class Float(channel: AsyncReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                        (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Float with FloatLike

  object Double extends AsyncBufferReaderFactory

  final case class Double(channel: AsyncReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                         (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Double with DoubleLike

  trait ByteLike extends AsyncBufferReader {
    me: BufferHandler.Byte =>

    final def read(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
      var remaining = len
      var position   = off
      while (remaining > 0) {
        val chunkLen  = min(bufFrames, remaining)
        val m			    = chunkLen * frameSize
        (byteBuf: Buffer).rewind().limit(m)
        val fut = channel.read(byteBuf)
        fut.map { _ =>
          byteBuf.flip()
          byteBuf.get(arrayBuf, 0, m)
          var ch = 0; while (ch < numChannels) {
            val b = frames(ch)
            if (b != null) {
              var i = ch; var j = position; while (i < m) {
                b(j) = arrayBuf(i).toFloat / 0x7F
                i += numChannels; j += 1
              }
            }
            ch += 1
          }
          remaining -= chunkLen
          position  += chunkLen
        }
      }
      ???
    }
  }

  trait UByteLike extends AsyncBufferReader {
    me: BufferHandler.UByte =>

    final def read(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen  = min(bufFrames, remaining)
        val m         = chunkLen * frameSize
        (byteBuf: Buffer).rewind().limit(m)
        channel.read(byteBuf)
        byteBuf.flip()
        byteBuf.get(arrayBuf, 0, m)
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              val arr1 = arrayBuf(i)
              // java can't handle unsigned bytes
              b(j) = (if (arr1 < 0) 0x80 + arr1 else arr1 - 0x80).toFloat / 0x7F
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }

  trait ShortLike extends AsyncBufferReader {
    me: BufferHandler.Short =>

    final def read(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen  = min(bufFrames, remaining)
        val m			    = chunkLen * numChannels
        (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
        channel.read(byteBuf)
        (viewBuf: Buffer).clear()
        viewBuf.get(arrayBuf, 0, m)
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              b(j) = arrayBuf(i).toFloat / 0x7FFF
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }

  trait ThreeBytesBELike extends AsyncBufferReader {
    me: BufferHandler.ThreeBytes =>

    final def read(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen  = min(bufFrames, remaining)
        val m			    = chunkLen * frameSize
        (byteBuf: Buffer).rewind().limit(m)
        channel.read(byteBuf)
        byteBuf.flip()
        byteBuf.get(arrayBuf, 0, m)
        var ch = 0; var p = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = p; var j = position; while (i < m) {
              b(j) = ((arrayBuf(i) << 16 ) |
                ((arrayBuf(i + 1) & 0xFF) << 8) |
                (arrayBuf(i + 2) & 0xFF)).toFloat / 0x7FFFFF
              i += chStep; j += 1
            }
          }
          ch += 1; p += 3
        }
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }

  trait ThreeBytesLELike extends AsyncBufferReader {
    me: BufferHandler.ThreeBytes =>

    final def read(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen  = min(bufFrames, remaining)
        val m			    = chunkLen * frameSize
        (byteBuf: Buffer).rewind().limit(m)
        channel.read(byteBuf)
        byteBuf.flip()
        byteBuf.get(arrayBuf, 0, m)
        var ch = 0; var p = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = p; var j = position; while (i < m) {
              b(j) = ((arrayBuf(i) & 0xFF)|
                ((arrayBuf(i + 1) & 0xFF) << 8) |
                (arrayBuf(i + 2) << 16 )).toFloat / 0x7FFFFF
              i += chStep; j += 1
            }
          }
          ch += 1; p += 3
        }
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }

  trait IntLike extends AsyncBufferReader {
    me: BufferHandler.Int =>

    final def read(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen   = min(bufFrames, remaining)
        val m			   = chunkLen * numChannels
        (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
        channel.read(byteBuf)
        (viewBuf: Buffer).clear()
        viewBuf.get(arrayBuf, 0, m)
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              b(j) = arrayBuf(i).toFloat / 0x7FFFFFFF
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }

  trait FloatLike extends AsyncBufferReader {
    me: BufferHandler.Float =>

    private final val sync = new AnyRef

    final def read(frames: Frames, off: SInt, len: SInt): Future[Unit] =
      if (len <= 0) Future.unit /*successful(())*/ else {
        val chunkLen   = min(bufFrames, len)
        val m          = chunkLen * numChannels
        val fut = sync.synchronized {
          (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
          channel.read(byteBuf)
        }
        fut.flatMap { _ =>
          // N.B.: "Buffers are not safe for use by multiple concurrent threads.
          // If a buffer is to be used by more than one thread then access to the buffer
          // should be controlled by appropriate synchronization."
          sync.synchronized {
            (viewBuf: Buffer).clear()
            // println(s"READ [2]: m = $m, viewBuf.limit = ${viewBuf.limit()}, _.position = ${viewBuf.position()}, _.remaining ${viewBuf.remaining()}")
            viewBuf.get(arrayBuf, 0, m)
          }
          var ch = 0
          while (ch < numChannels) {
            val b = frames(ch)
            if (b != null) {
              var i = ch; var j = off; while (i < m) {
                b(j) = arrayBuf(i)
                i += numChannels; j += 1
              }
            }
            ch += 1
          }
          read(frames, off = off + chunkLen, len = len - chunkLen)
        }
      }
  }

  trait DoubleLike extends AsyncBufferReader {
    me: BufferHandler.Double =>

    final def read(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen   = math.min(bufFrames, remaining)
        val m			   = chunkLen * numChannels
        (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
        channel.read(byteBuf)
        (viewBuf: Buffer).clear()
        viewBuf.get(arrayBuf, 0, m)
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = ch; var j = position; while (i < m) {
              b(j) = arrayBuf(i).toFloat
              i += numChannels; j += 1
            }
          }
          ch += 1
        }
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }
}

// -------------- Write --------------

private[io] trait AsyncBufferWriterFactory {
  def apply(write: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
           (implicit ec: ExecutionContext): AsyncBufferWriter
}

private[io] object AsyncBufferWriter {

  object Byte extends AsyncBufferWriterFactory

  final case class Byte(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                       (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Byte with ByteLike

  // float to byte = f*0x7F+0x80 (-1 ... +1becomes 0x01 to 0xFF)
  // which is how libsndfile behaves
  object UByte extends AsyncBufferWriterFactory

  final case class UByte(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                        (implicit val executionContext: ExecutionContext)
    extends BufferHandler.UByte with UByteLike {
    //      checkCapacity()
  }

  object Short extends AsyncBufferWriterFactory

  final case class Short(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                        (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Short with ShortLike {
    //      checkCapacity()
  }

  object ThreeBytes extends AsyncBufferWriterFactory {
    def apply(write: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
             (implicit ec: ExecutionContext): AsyncBufferWriter =
      if (byteBuf.order() == ByteOrder.LITTLE_ENDIAN) {
        ThreeBytesLE(write, byteBuf, numChannels)
      } else {
        ThreeBytesBE(write, byteBuf, numChannels)
      }
  }

  final case class ThreeBytesBE(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                               (implicit val executionContext: ExecutionContext)
    extends BufferHandler.ThreeBytes with ThreeBytesBELike

  final case class ThreeBytesLE(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                               (implicit val executionContext: ExecutionContext)
    extends BufferHandler.ThreeBytes with ThreeBytesLELike

  object Int extends AsyncBufferWriterFactory

  final case class Int(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                      (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Int with IntLike

  object Float extends AsyncBufferWriterFactory

  final case class Float(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                        (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Float with FloatLike

  object Double extends AsyncBufferWriterFactory

  final case class Double(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
                         (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Double with DoubleLike

  trait ByteLike extends AsyncBufferWriter {
    me: BufferHandler.Byte =>

    final def write(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
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
        byteBuf.flip()
        channel.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }

  trait UByteLike extends AsyncBufferWriter {
    me: BufferHandler.UByte =>

    final def write(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
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
        byteBuf.flip()
        channel.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }

  trait ShortLike extends AsyncBufferWriter {
    me: BufferHandler.Short =>

    final def write(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
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
        channel.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }

  trait ThreeBytesBELike extends AsyncBufferWriter {
    me: BufferHandler.ThreeBytes =>

    final def write(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
      var remaining = len
      var position = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * frameSize
        var ch = 0; var p = 0; while (ch < numChannels) {
          val b = frames(ch)
          var i = p; var j = position; while (i < m) {
            val k = (b(j) * 0x7FFFFF).toInt
            arrayBuf(i)     = (k >> 16).toByte
            arrayBuf(i + 1) = (k >> 8).toByte
            arrayBuf(i + 2) = k.toByte
            i += chStep; j += 1
          }
          ch += 1; p += 3
        }
        (byteBuf: Buffer).clear()
        byteBuf.put(arrayBuf, 0, m)
        byteBuf.flip()
        channel.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }

  trait ThreeBytesLELike extends AsyncBufferWriter {
    me: BufferHandler.ThreeBytes =>

    final def write(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
      var remaining = len
      var position  = off
      while (remaining > 0) {
        val chunkLen  = math.min(bufFrames, remaining)
        val m			    = chunkLen * frameSize
        var ch = 0; var p = 0; while (ch < numChannels) {
          val b = frames(ch)
          if (b != null) {
            var i = p; var j = position; while (i < m) {
              val k = (b(j) * 0x7FFFFF).toInt
              arrayBuf(i)     = k.toByte
              arrayBuf(i + 1) = (k >> 8).toByte
              arrayBuf(i + 2) = (k >> 16).toByte
              i += chStep; j += 1
            }
          }
          ch += 1; p += 3
        }
        (byteBuf: Buffer).clear()
        byteBuf.put(arrayBuf, 0, m)
        byteBuf.flip()
        channel.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }

  trait IntLike extends AsyncBufferWriter {
    me: BufferHandler.Int =>

    final def write(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
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
        channel.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }

  trait FloatLike extends AsyncBufferWriter {
    me: BufferHandler.Float =>

    private final val sync = new AnyRef

    final def write(frames: Frames, off: SInt, len: SInt): Future[Unit] =
      if (len <= 0) Future.unit /*successful(())*/ else {
        val chunkLen  = min(bufFrames, len)
        val m			    = chunkLen * numChannels
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          var i = ch; var j = off; while (i < m) {
            arrayBuf(i) = b(j)
            i += numChannels; j += 1
          }
          ch += 1
        }
        // N.B.: "Buffers are not safe for use by multiple concurrent threads.
        // If a buffer is to be used by more than one thread then access to the buffer
        // should be controlled by appropriate synchronization."
        val fut = sync.synchronized {
          (viewBuf: Buffer).clear()
          viewBuf.put(arrayBuf, 0, m)
          (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
          channel.write(byteBuf)
        }
        fut.flatMap { _ =>
          write(frames, off = off + chunkLen, len = len - chunkLen)
        }
      }
  }

  trait DoubleLike extends AsyncBufferWriter {
    me: BufferHandler.Double =>

    final def write(frames: Frames, off: SInt, len: SInt): Future[Unit] = {
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
        channel.write(byteBuf)
        remaining -= chunkLen
        position  += chunkLen
      }
      ???
    }
  }
}

// -------------- Handler -------------- 

private[io] trait AsyncBufferBidiFactory {
  def apply(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
           (implicit ec: ExecutionContext): AsyncBufferBidi
}

private[io] object AsyncBufferBidi {

  object Byte extends AsyncBufferBidiFactory

  final case class Byte(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer,
                        numChannels: SInt)
                       (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Byte with AsyncBufferReader.ByteLike with AsyncBufferWriter.ByteLike
      with AsyncBufferBidi

  object UByte extends AsyncBufferBidiFactory

  final case class UByte(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer,
                         numChannels: SInt)
                        (implicit val executionContext: ExecutionContext)
    extends BufferHandler.UByte with AsyncBufferReader.UByteLike with AsyncBufferWriter.UByteLike
      with AsyncBufferBidi

  object Short extends AsyncBufferBidiFactory

  final case class Short(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer,
                         numChannels: SInt)
                        (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Short with AsyncBufferReader.ShortLike with AsyncBufferWriter.ShortLike
      with AsyncBufferBidi

  object ThreeBytes extends AsyncBufferBidiFactory {
    def apply(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer,
              numChannels: SInt)
             (implicit ec: ExecutionContext): AsyncBufferBidi =
      if (byteBuf.order() == ByteOrder.LITTLE_ENDIAN) {
        ThreeBytesLE(channel, byteBuf, numChannels)
      } else {
        ThreeBytesBE(channel, byteBuf, numChannels)
      }
  }

  final case class ThreeBytesBE(channel: AsyncWritableByteChannel,
                                byteBuf: ByteBuffer, numChannels: SInt)
                               (implicit val executionContext: ExecutionContext)
    extends BufferHandler.ThreeBytes with AsyncBufferReader.ThreeBytesBELike with AsyncBufferWriter.ThreeBytesBELike
      with AsyncBufferBidi

  final case class ThreeBytesLE(channel: AsyncWritableByteChannel,
                                byteBuf: ByteBuffer, numChannels: SInt)
                               (implicit val executionContext: ExecutionContext)
    extends BufferHandler.ThreeBytes with AsyncBufferReader.ThreeBytesLELike with AsyncBufferWriter.ThreeBytesLELike
      with AsyncBufferBidi

  object Int extends AsyncBufferBidiFactory

  final case class Int(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer,
                       numChannels: SInt)
                      (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Int with AsyncBufferReader.IntLike with AsyncBufferWriter.IntLike
      with AsyncBufferBidi

  object Float extends AsyncBufferBidiFactory

  final case class Float(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer,
                         numChannels: SInt)
                        (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Float with AsyncBufferReader.FloatLike with AsyncBufferWriter.FloatLike
      with AsyncBufferBidi

  object Double extends AsyncBufferBidiFactory

  final case class Double(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer,
                          numChannels: SInt)
                         (implicit val executionContext: ExecutionContext)
    extends BufferHandler.Double with AsyncBufferReader.DoubleLike with AsyncBufferWriter.DoubleLike
      with AsyncBufferBidi
}
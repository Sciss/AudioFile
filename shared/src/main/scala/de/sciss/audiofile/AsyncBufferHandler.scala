/*
 *  AsyncBufferHandler.scala
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
import java.nio.{Buffer, ByteBuffer, ByteOrder}

import AudioFile.Frames
import de.sciss.asyncfile.{AsyncReadableByteChannel, AsyncWritableByteChannel}

import scala.concurrent.{ExecutionContext, Future}
import scala.{Double => SDouble, Int => SInt}
import scala.math.min

private[audiofile] trait AsyncBufferHandler extends BufferHandler {
  implicit def executionContext: ExecutionContext
}

private[audiofile] trait AsyncBufferReader extends AsyncBufferHandler {
  @throws(classOf[IOException])
  def read(frames: Frames, off: SInt, len: SInt): Future[Unit]

  val channel: AsyncReadableByteChannel
}

private[audiofile] trait AsyncBufferWriter extends AsyncBufferHandler {
  @throws(classOf[IOException])
  def write(frames: Frames, off: SInt, len: SInt): Future[Unit]

  val channel: AsyncWritableByteChannel
}

private[audiofile] trait AsyncBufferBidi extends AsyncBufferReader with AsyncBufferWriter

// -------------- Reader --------------

private[audiofile] trait AsyncBufferReaderFactory {
  def apply(read: AsyncReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
           (implicit ec: ExecutionContext): AsyncBufferReader
}

private[audiofile] object AsyncBufferReader {

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

  trait Base extends AsyncBufferReader {

    private final val sync = new AnyRef

    protected def arrayToSamples(smp: Array[SDouble], smpOff: SInt, arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit

    protected def bufferToArray(numSmp: SInt): Unit

    final def read(frames: Frames, off: SInt, len: SInt): Future[Unit] =
      if (len <= 0) Future.unit else {
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
            bufferToArray(m)
          }
          var ch = 0
          while (ch < numChannels) {
            val b = frames(ch)
            if (b != null) {
              arrayToSamples(smp = b, smpOff = off, arrOff = ch, arrStep = numChannels, arrStop = m)
            }
            ch += 1
          }
          read(frames, off = off + chunkLen, len = len - chunkLen)
        }
      }
  }

  trait ByteLike extends Base with AsyncBufferReader {
    me: BufferHandler.Byte =>

    protected def arrayToSamples(smp: Array[SDouble], smpOff: SInt,
                                 arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff; var j = smpOff; while (i < arrStop) {
        smp(j) = arrayBuf(i).toDouble / 0x7F
        i += arrStep; j += 1
      }
    }

    protected def bufferToArray(numSmp: SInt): Unit = {
      (byteBuf: Buffer).clear()
      byteBuf.get(arrayBuf, 0, numSmp)
      ()
    }
  }

  trait UByteLike extends Base with AsyncBufferReader {
    me: BufferHandler.UByte =>

    protected def arrayToSamples(smp: Array[SDouble], smpOff: SInt,
                                 arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff; var j = smpOff; while (i < arrStop) {
        val arr1 = arrayBuf(i)
        // java can't handle unsigned bytes
        smp(j) = (if (arr1 < 0) 0x80 + arr1 else arr1 - 0x80).toDouble / 0x7F
        i += arrStep; j += 1
      }
    }

    protected def bufferToArray(numSmp: SInt): Unit = {
      (byteBuf: Buffer).clear()
      byteBuf.get(arrayBuf, 0, numSmp)
      ()
    }
  }

  trait ShortLike extends Base with AsyncBufferReader {
    me: BufferHandler.Short =>

    protected def arrayToSamples(smp: Array[SDouble], smpOff: SInt,
                                 arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff; var j = smpOff; while (i < arrStop) {
        smp(j) = arrayBuf(i).toDouble / 0x7FFF
        i += arrStep; j += 1
      }
    }

    protected def bufferToArray(numSmp: SInt): Unit = {
      (viewBuf: Buffer).clear()
      viewBuf.get(arrayBuf, 0, numSmp)
      ()
    }
  }

  trait ThreeBytesBELike extends Base with AsyncBufferReader {
    me: BufferHandler.ThreeBytes =>

    protected def arrayToSamples(smp: Array[SDouble], smpOff: SInt,
                                 arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff * 3; val iStep = arrStep * 3; val iStop = arrStop * 3; var j = smpOff; while (i < iStop) {
        smp(j) = (
           (arrayBuf(i)             << 16) |
          ((arrayBuf(i + 1) & 0xFF) <<  8) |
           (arrayBuf(i + 2) & 0xFF)
          ).toDouble / 0x7FFFFF
        i += iStep; j += 1
      }
    }

    protected def bufferToArray(numSmp: SInt): Unit = {
      (byteBuf: Buffer).clear()
      byteBuf.get(arrayBuf, 0, numSmp * 3)
      ()
    }
  }

  trait ThreeBytesLELike extends Base with AsyncBufferReader {
    me: BufferHandler.ThreeBytes =>

    protected def arrayToSamples(smp: Array[SDouble], smpOff: SInt,
                                 arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff * 3; val iStep = arrStep * 3; val iStop = arrStop * 3; var j = smpOff; while (i < iStop) {
        smp(j) = (
             (arrayBuf(i)     & 0xFF)        |
            ((arrayBuf(i + 1) & 0xFF) <<  8) |
             (arrayBuf(i + 2)         << 16)
          ).toDouble / 0x7FFFFF
        i += iStep; j += 1
      }
    }

    protected def bufferToArray(numSmp: SInt): Unit = {
      (byteBuf: Buffer).clear()
      byteBuf.get(arrayBuf, 0, numSmp * 3)
      ()
    }
  }

  trait IntLike extends Base with AsyncBufferReader {
    me: BufferHandler.Int =>

    protected def arrayToSamples(smp: Array[SDouble], smpOff: SInt,
                                 arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff; var j = smpOff; while (i < arrStop) {
        smp(j) = arrayBuf(i).toDouble / 0x7FFFFFFF
        i += arrStep; j += 1
      }
    }

    protected def bufferToArray(numSmp: SInt): Unit = {
      (viewBuf: Buffer).clear()
      viewBuf.get(arrayBuf, 0, numSmp)
      ()
    }
  }

  trait FloatLike extends Base with AsyncBufferReader {
    me: BufferHandler.Float =>

    protected def arrayToSamples(smp: Array[SDouble], smpOff: SInt,
                                 arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff; var j = smpOff; while (i < arrStop) {
        smp(j) = arrayBuf(i)
        i += arrStep; j += 1
      }
    }

    protected def bufferToArray(numSmp: SInt): Unit = {
      (viewBuf: Buffer).clear()
      viewBuf.get(arrayBuf, 0, numSmp)
      ()
    }
  }

  trait DoubleLike extends Base with AsyncBufferReader {
    me: BufferHandler.Double =>

    protected def arrayToSamples(smp: Array[SDouble], smpOff: SInt,
                                 arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff; var j = smpOff; while (i < arrStop) {
        smp(j) = arrayBuf(i).toFloat
        i += arrStep; j += 1
      }
    }

    protected def bufferToArray(numSmp: SInt): Unit = {
      (viewBuf: Buffer).clear()
      viewBuf.get(arrayBuf, 0, numSmp)
      ()
    }
  }
}

// -------------- Write --------------

private[audiofile] trait AsyncBufferWriterFactory {
  def apply(write: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
           (implicit ec: ExecutionContext): AsyncBufferWriter
}

private[audiofile] object AsyncBufferWriter {

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

  trait Base extends AsyncBufferWriter {
    private final val sync = new AnyRef

    protected def samplesToArray(smp: Array[SDouble], smpOff: SInt, arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit

    protected def arrayToBuffer(numSmp: SInt): Unit

    final def write(frames: Frames, off: SInt, len: SInt): Future[Unit] =
      if (len <= 0) Future.unit else {
        val chunkLen  = min(bufFrames, len)
        val m			    = chunkLen * numChannels
        var ch = 0; while (ch < numChannels) {
          val b = frames(ch)
          samplesToArray(smp = b, smpOff = off, arrOff = ch, arrStep = numChannels, arrStop = m)
          ch += 1
        }
        // N.B.: "Buffers are not safe for use by multiple concurrent threads.
        // If a buffer is to be used by more than one thread then access to the buffer
        // should be controlled by appropriate synchronization."
        val fut = sync.synchronized {
          arrayToBuffer(m)
          (byteBuf: Buffer).rewind().limit(chunkLen * frameSize)
          channel.write(byteBuf)
        }
        fut.flatMap { _ =>
          write(frames, off = off + chunkLen, len = len - chunkLen)
        }
      }
  }

  trait ByteLike extends Base with AsyncBufferWriter {
    me: BufferHandler.Byte =>

    protected final def samplesToArray(smp: Array[SDouble], smpOff: SInt,
                                       arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff; var j = smpOff; while (i < arrStop) {
        arrayBuf(i) = (smp(j) * 0x7F).toByte
        i += arrStep; j += 1
      }
    }

    protected final def arrayToBuffer(numSmp: SInt): Unit = {
      (byteBuf: Buffer).clear()
      byteBuf.put(arrayBuf, 0, numSmp)
      ()
    }
  }

  trait UByteLike extends Base with AsyncBufferWriter {
    me: BufferHandler.UByte =>

    protected final def samplesToArray(smp: Array[SDouble], smpOff: SInt,
                                       arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff; var j = smpOff; while (i < arrStop) {
        arrayBuf(i) = (smp(j) * 0x7F + 0x80).toByte
        i += arrStep; j += 1
      }
    }

    protected final def arrayToBuffer(numSmp: SInt): Unit = {
      (byteBuf: Buffer).clear()
      byteBuf.put(arrayBuf, 0, numSmp)
      ()
    }
  }

  trait ShortLike extends Base with AsyncBufferWriter {
    me: BufferHandler.Short =>

    protected final def samplesToArray(smp: Array[SDouble], smpOff: SInt,
                                       arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff; var j = smpOff; while (i < arrStop) {
        arrayBuf(i) = (smp(j) * 0x7FFF).toShort
        i += arrStep; j += 1
      }
    }

    protected final def arrayToBuffer(numSmp: SInt): Unit = {
      (viewBuf: Buffer).clear()
      viewBuf.put(arrayBuf, 0, numSmp)
      ()
    }
  }

  trait ThreeBytesBELike extends Base with AsyncBufferWriter {
    me: BufferHandler.ThreeBytes =>

    protected final def samplesToArray(smp: Array[SDouble], smpOff: SInt,
                                       arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff * 3; val iStep = arrStep * 3; val iStop = arrStop * 3; var j = smpOff; while (i < iStop) {
        val k = (smp(j) * 0x7FFFFF).toInt
        arrayBuf(i)     = (k >> 16).toByte
        arrayBuf(i + 1) = (k >>  8).toByte
        arrayBuf(i + 2) = k.toByte
        i += iStep; j += 1
      }
    }

    protected final def arrayToBuffer(numSmp: SInt): Unit = {
      (byteBuf: Buffer).clear()
      byteBuf.put(arrayBuf, 0, numSmp * 3)
      ()
    }
  }

  trait ThreeBytesLELike extends Base with AsyncBufferWriter {
    me: BufferHandler.ThreeBytes =>

    protected final def samplesToArray(smp: Array[SDouble], smpOff: SInt,
                                       arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff * 3; val iStep = arrStep * 3; val iStop = arrStop * 3; var j = smpOff; while (i < iStop) {
        val k = (smp(j) * 0x7FFFFF).toInt
        arrayBuf(i)     = k.toByte
        arrayBuf(i + 1) = (k >>  8).toByte
        arrayBuf(i + 2) = (k >> 16).toByte
        i += iStep; j += 1
      }
    }

    protected final def arrayToBuffer(numSmp: SInt): Unit = {
      (byteBuf: Buffer).clear()
      byteBuf.put(arrayBuf, 0, numSmp * 3)
      ()
    }
  }

  trait IntLike extends Base with AsyncBufferWriter {
    me: BufferHandler.Int =>

    protected final def samplesToArray(smp: Array[SDouble], smpOff: SInt,
                                       arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff; var j = smpOff; while (i < arrStop) {
        arrayBuf(i) = (smp(j) * 0x7FFFFFFF).toInt
        i += arrStep; j += 1
      }
    }

    protected final def arrayToBuffer(numSmp: SInt): Unit = {
      (viewBuf: Buffer).clear()
      viewBuf.put(arrayBuf, 0, numSmp)
      ()
    }
  }

  trait FloatLike extends Base with AsyncBufferWriter {
    me: BufferHandler.Float =>

    protected final def samplesToArray(smp: Array[SDouble], smpOff: SInt,
                                       arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff; var j = smpOff; while (i < arrStop) {
        arrayBuf(i) = smp(j).toFloat
        i += arrStep; j += 1
      }
    }

    protected final def arrayToBuffer(numSmp: SInt): Unit = {
      (viewBuf: Buffer).clear()
      viewBuf.put(arrayBuf, 0, numSmp)
      ()
    }
  }

  trait DoubleLike extends Base with AsyncBufferWriter {
    me: BufferHandler.Double =>

    protected final def samplesToArray(smp: Array[SDouble], smpOff: SInt,
                                       arrOff: SInt, arrStep: SInt, arrStop: SInt): Unit = {
      var i = arrOff; var j = smpOff; while (i < arrStop) {
        arrayBuf(i) = smp(j)
        i += arrStep; j += 1
      }
    }

    protected final def arrayToBuffer(numSmp: SInt): Unit = {
      (viewBuf: Buffer).clear()
      viewBuf.put(arrayBuf, 0, numSmp)
      ()
    }
  }
}

// -------------- Handler -------------- 

private[audiofile] trait AsyncBufferBidiFactory {
  def apply(channel: AsyncWritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt)
           (implicit ec: ExecutionContext): AsyncBufferBidi
}

private[audiofile] object AsyncBufferBidi {

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
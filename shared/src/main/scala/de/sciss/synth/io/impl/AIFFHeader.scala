/*
 *  AIFFHeader.scala
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
package impl

import java.io.{DataInput, DataInputStream, DataOutput, DataOutputStream, EOFException, IOException, RandomAccessFile}
import java.nio.ByteOrder.{BIG_ENDIAN, LITTLE_ENDIAN}
import java.nio.{Buffer, ByteBuffer, ByteOrder}
import java.util.ConcurrentModificationException

import de.sciss.serial.impl.ByteArrayOutputStream

import scala.annotation.switch
import scala.concurrent.Future
import scala.math.{pow, signum}

private[io] object AIFFHeader extends BasicHeader {
  private final val FORM_MAGIC = 0x464F524D     // 'FORM'
  private final val AIFF_MAGIC = 0x41494646     // 'AIFF'   (off 8)
  private final val AIFC_MAGIC = 0x41494643     // 'AIFC'   (off 8)

  // chunk identifiers
  private final val COMM_MAGIC = 0x434F4D4D     // 'COMM'
  //   private final val INST_MAGIC		= 0x494E5354	// 'INST'
  //   private final val MARK_MAGIC		= 0x4D41524B	// 'MARK'
  private final val SSND_MAGIC = 0x53534E44     // 'SSND
  private final val FVER_MAGIC = 0x46564552     // 'FVER
  //   private final val APPL_MAGIC		= 0x4150504C	// 'APPL'
  //   private final val COMT_MAGIC		= 0x434F4D54	// 'COMT'
  //   private final val ANNO_MAGIC		= 0x414E4E4F	// 'ANNO'

  // aifc compression identifiers
  private final val NONE_MAGIC = 0x4E4F4E45     // 'NONE' (AIFC-compression)
  private final val fl32_MAGIC = 0x666C3332     // 'fl32' (AIFC-compression)
  private final val FL32_MAGIC = 0x464C3332     // SoundHack variant
  private final val fl64_MAGIC = 0x666C3634
  private final val FL64_MAGIC = 0x464C3634     // SoundHack variant
  private final val in16_MAGIC = 0x696E3136     // we "love" SoundHack for its special interpretations
  private final val in24_MAGIC = 0x696E3234
  private final val in32_MAGIC = 0x696E3332
  private final val in16LE_MAGIC = 0x736F7774   // 'sowt' (16-bit PCM little endian)

  private final val AIFCVersion1 = 0xA2805140   // FVER chunk
  // private val NONE_HUMAN	   = "uncompressed"
  //   private val fl32_HUMAN	   = Array[Byte]( 12, 51, 50, 45, 98, 105, 116, 32, 102, 108, 111, 97, 116, 0 ) // "32-bit float"
  //   private val fl64_HUMAN	   = Array[Byte]( 12, 54, 52, 45, 98, 105, 116, 32, 102, 108, 111, 97, 116, 0 ) // "64-bit float"
  //   private val in16_HUMAN	   = Array[Byte]( 12, 49, 54, 45, 98, 105, 116, 32, 105, 110, 116, 32,  32, 0 ) // "16-bit int  "

  // "32-bit float"
  private final val fl32_HUMAN = Array[Byte](
    102, 108, 51, 50, 12,
    51, 50, 45, 98, 105, 116, 32, 102, 108, 111, 97, 116, 0)

  // "64-bit float"
  private final val fl64_HUMAN = Array[Byte](
    102, 108, 54, 52, 12,
    54, 52, 45, 98, 105, 116, 32, 102, 108, 111, 97, 116, 0)

  // "16-bit int  "
  private final val in16_HUMAN = Array[Byte](
    115, 111, 119, 116, 12,
    49, 54, 45, 98, 105, 116, 32, 105, 110, 116, 32, 32, 0)

  private final val LN2R = 1.0 / math.log(2)

  @throws(classOf[IOException])
  def identify(dis: DataInputStream): Boolean = {
    val cookie = dis.readInt()
    if (cookie == FORM_MAGIC) {
      // -------- probably AIFF --------
      dis.readInt()
      val magic = dis.readInt()
      magic == AIFC_MAGIC || magic == AIFF_MAGIC
    } else false
  }

  import AudioFileHeader._

  @throws(classOf[IOException])
  protected def readDataInput(din: DataInput): AudioFileHeader = {
    if (din.readInt() != FORM_MAGIC) formatError() // FORM
    // trust the file len more than 32 bit form field which
    // breaks for > 2 GB (> 1 GB if using signed ints)
    din.readInt()
    //       var len           = dis.length() - 8
    //			var len           = (dis.readInt() + 1).toLong & 0xFFFFFFFEL // this gives 32 bit unsigned space (4 GB)
    val isAIFC = (din.readInt(): @switch) match {
      case AIFC_MAGIC => true
      case AIFF_MAGIC => false
      case _          => formatError()
    }
    //         len	           -= 4
    var chunkLen = 0 // updated per chunk; after each chunk we skip the remaining bytes

    // these we need...
    var afh: AudioFileHeader = null
    var ssndFound = false

    try {
      while (!ssndFound) {
        if (chunkLen != 0) din.skipBytes(chunkLen) // skip remainder from previous chunk

        val magic = din.readInt()
        chunkLen  = (din.readInt() + 1) & 0xFFFFFFFE

        magic match {
          case COMM_MAGIC =>
            // reveals spec
            val numChannels   = din.readShort()
            // commSmpNumOffset  = dis.getFilePointer()
            val numFrames     = din.readInt().toLong & 0xFFFFFFFFL
            val bitsPerSample = din.readShort()

            // the most complicated data format to store a float:
            val l1 = din.readLong()
            val l2 = din.readUnsignedShort()
            val l3 = l1 & 0x0000FFFFFFFFFFFFL
            val i1 = ((l1 >> 48).toInt & 0x7FFF) - 0x3FFE
            val sampleRate = ((l3 * pow(2.0, i1 - 48)) +
                              (l2 * pow(2.0, i1 - 64))) * signum(l1)

            chunkLen -= 18
            val (byteOrder, sampleFormat) = if (isAIFC) {
              chunkLen -= 4
              (din.readInt(): @switch) match {
                case `NONE_MAGIC`   => (BIG_ENDIAN, intSampleFormat(bitsPerSample))
                case `in16_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Int16 )
                case `in24_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Int24 )
                case `in32_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Int32 )
                case `fl32_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Float )
                case `FL32_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Float )
                case `fl64_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Double)
                case `FL64_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Double)
                case `in16LE_MAGIC` => (LITTLE_ENDIAN , SampleFormat.Int16 )
                case m => throw new IOException(s"Unsupported AIFF encoding ($m)")
              }
            } else (BIG_ENDIAN, intSampleFormat(bitsPerSample))

            val spec  = AudioFileSpec(fileType = AudioFileType.AIFF, sampleFormat = sampleFormat,
              numChannels = numChannels, sampleRate = sampleRate, byteOrder = Some(byteOrder), numFrames = numFrames)
            afh       = ReadableAudioFileHeader(spec, byteOrder)


          // case INST_MAGIC =>
          // case MARK_MAGIC =>

          case SSND_MAGIC =>
            val i1 = din.readInt() // sample data off
            din.readInt()
            din.skipBytes(i1)
            ssndFound = true // important: this must be the last case block statement coz we catch EOF!

          // case APPL_MAGIC =>
          // case COMT_MAGIC =>
          // case ANNO_MAGIC =>

          case _ => // ignore unknown chunks
        } // magic match
      } // essentials loop
    } catch {
      case _: EOFException =>
    }

    if (afh == null) throw new IOException("AIFF header misses COMM chunk")
    if (!ssndFound)  throw new IOException("AIFF header misses SSND chunk")

    afh
  }

  def readAsync(ch: AsyncReadableByteChannel): Future[AudioFileHeader] = {
    val ab = new AsyncReadableByteBuffer(ch)
    import ab._

    ensure(12).flatMap { _ =>
      if (bb.getInt() != FORM_MAGIC) formatError() // FORM
      // trust the file len more than 32 bit form field which
      // breaks for > 2 GB (> 1 GB if using signed ints)
      bb.getInt()
      //       var len           = dis.length() - 8
      //			var len           = (dis.readInt() + 1).toLong & 0xFFFFFFFEL // this gives 32 bit unsigned space (4 GB)
      val isAIFC = (bb.getInt(): @switch) match {
        case AIFC_MAGIC => true
        case AIFF_MAGIC => false
        case _          => formatError()
      }

      def readChunk(afh: AudioFileHeader): Future[AudioFileHeader] = {
        ensure(8).flatMap { _ =>
          val magic     = bb.getInt()
          var chunkLen  = (bb.getInt() + 1) & 0xFFFFFFFE

          magic match {
            case COMM_MAGIC =>
              ensure(if (isAIFC) 22 else 18).flatMap { _ =>
                // reveals spec
                val numChannels   = bb.getShort()                       // 2
                val numFrames     = bb.getInt().toLong & 0xFFFFFFFFL    // 6
                val bitsPerSample = bb.getShort()                       // 8

                // the most complicated data format to store a float:
                val l1 = bb.getLong()                                   // 16
                val l2 = bb.getShort() & 0xFFFF                         // 18
                val l3 = l1 & 0x0000FFFFFFFFFFFFL
                val i1 = ((l1 >> 48).toInt & 0x7FFF) - 0x3FFE
                val sampleRate = ((l3 * pow(2.0, i1 - 48)) +
                  (l2 * pow(2.0, i1 - 64))) * signum(l1)

                chunkLen -= 18
                val (byteOrder, sampleFormat) = if (isAIFC) {
                  chunkLen -= 4
                  (bb.getInt(): @switch) match {                        // 22
                    case `NONE_MAGIC`   => (BIG_ENDIAN, intSampleFormat(bitsPerSample))
                    case `in16_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Int16 )
                    case `in24_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Int24 )
                    case `in32_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Int32 )
                    case `fl32_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Float )
                    case `FL32_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Float )
                    case `fl64_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Double)
                    case `FL64_MAGIC`   => (BIG_ENDIAN    , SampleFormat.Double)
                    case `in16LE_MAGIC` => (LITTLE_ENDIAN , SampleFormat.Int16 )
                    case m => throw new IOException(s"Unsupported AIFF encoding ($m)")
                  }
                } else (BIG_ENDIAN, intSampleFormat(bitsPerSample))

                val spec  = AudioFileSpec(fileType = AudioFileType.AIFF, sampleFormat = sampleFormat,
                  numChannels = numChannels, sampleRate = sampleRate, byteOrder = Some(byteOrder), numFrames = numFrames)
                val _afh = ReadableAudioFileHeader(spec, byteOrder)
                skip(chunkLen)
                readChunk(_afh)
              }

            case SSND_MAGIC =>
              ensure(4).map { _ =>
                val i1 = bb.getInt() // sample data off
                bb.getInt()
                skip(i1)
                purge()
                if (afh != null) afh else throw new IOException("AIFF header misses COMM chunk")
              }

            case _ => // ignore unknown chunks
              skip(chunkLen)
              readChunk(afh)
          }
        }
      }

      readChunk(null)
    }
  }

  /*
   *    WARNING: it is crucial to add the return type here
   *    (see scala ticket #3440)
   */
  private def intSampleFormat(bitsPerSample: Int): SampleFormat = (bitsPerSample: @switch) match {
    case 8   => SampleFormat.Int8
    case 16  => SampleFormat.Int16
    case 24  => SampleFormat.Int24
    case 32  => SampleFormat.Int32
    case v   => throw new IOException(s"Unsupported AIFF encoding ($v bits-per-sample)")
  }

  @throws(classOf[IOException])
  def write(raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = {
    val writeRes = writeDataOutput(raf, spec, writeSize = false)
    import writeRes._
    new WritableFileHeader(raf, spec1, otherLen = otherLen, commLen = commLen)
  }

  @throws(classOf[IOException])
  def write(dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader = {
    val writeRes = writeDataOutput(dos, spec, writeSize = true)
    new NonUpdatingWritableHeader(writeRes.spec1)
  }

  def writeAsync(ch: AsyncWritableByteChannel, spec: AudioFileSpec): Future[AsyncWritableAudioFileHeader] = {
    val bs        = new ByteArrayOutputStream()
    val dout      = new DataOutputStream(bs)
    val writeRes  = writeDataOutput(dout, spec, writeSize = false)
    val dst       = ByteBuffer.wrap(bs.buffer, 0, bs.size)
    val fut       = ch.write(dst)
    import ch.executionContext
    fut.map { _ =>
      import writeRes._
      new AsyncWritableFileHeader(ch, spec1, otherLen = otherLen, commLen = commLen)
    }
  }

  @throws(classOf[IOException])
  private def writeDataOutput(dout: DataOutput, spec: AudioFileSpec, writeSize: Boolean): WriteHeaderResult = {
    val smpForm       = spec.sampleFormat
    val bitsPerSample = smpForm.bitsPerSample
    val sr            = spec.sampleRate
    val numFrames     = if (writeSize) spec.numFrames else 0L // initial value, only needed for stream files
    val numChannels   = spec.numChannels
    val byteOrder     = spec.byteOrder.getOrElse(ByteOrder.BIG_ENDIAN)

    val le16 = if (byteOrder == ByteOrder.LITTLE_ENDIAN) {
      if (smpForm != SampleFormat.Int16) throw new IOException("AIFF little endian only supported for Int16")
      true
    } else false

    val aifcExt: Array[Byte] = if (smpForm == SampleFormat.Float) {
      fl32_HUMAN
    } else if (smpForm == SampleFormat.Double) {
      fl64_HUMAN
    } else if (le16) {
      in16_HUMAN
    } else null

    val isAIFC = aifcExt != null  // floating point requires AIFC compression extension

    val otherLen  = if (isAIFC) 24 else 12 // FORM, MAGIC and FVER
    val commLen   = if (isAIFC) 26 + aifcExt.length else 26 // 8 + 2 + 4 + 2 + 10 + (isAIFC ? 4 + fl32_HUMAN.length : 0)
    val ssndLen   = (bitsPerSample >> 3) * numFrames * numChannels + 16
    val fileLen   = otherLen + commLen + ssndLen

    dout.writeInt(FORM_MAGIC)
    dout.writeInt((fileLen - 8).toInt) // length except FORM-Header (file size minus 8)

    // MAGIC and FVER Chunk
    if (isAIFC) {
      dout.writeInt(AIFC_MAGIC)
      dout.writeInt(FVER_MAGIC)
      dout.writeInt(4)
      dout.writeInt(AIFCVersion1)
    } else {
      dout.writeInt(AIFF_MAGIC)
    }

    // COMM Chunk
    dout.writeInt(COMM_MAGIC)
    //         pos = dis.getFilePointer();
    //         dis.writeInt( 0 );				// not known yet
    dout.writeInt(commLen - 8)

    dout.writeShort(numChannels)
    //         commSmpNumOffset = dis.getFilePointer();
    dout.writeInt(numFrames.toInt) // updated later
    dout.writeShort(if (isAIFC) 16 else bitsPerSample) // a quite strange convention ...

    // suckers never die.
    val srs = if (sr < 0.0) 0x80 else 0
    val sra = math.abs(sr)
    val srl = (math.log(sra) * LN2R + 0x3FFF).toInt & 0xFFFF
    val sre = sra * (1 << (0x401E - srl))
    dout.writeShort((((srs | (srl >> 8)) & 0xFF) << 8) | (srl & 0xFF))
    dout.writeInt(sre.toLong.toInt) // WARNING: a direct sre.toInt yields a wrong result (Int.MaxValue)
    dout.writeInt(((sre % 1.0) * 0x100000000L).toLong.toInt) // WARNING: same here

    if (isAIFC) {
      dout.write(aifcExt)
    }

    // SSND Chunk (Header)
    dout.writeInt(SSND_MAGIC)
    //         ssndLengthOffset = dis.getFilePointer();
    dout.writeInt((ssndLen - 8).toInt) // 8 + stream.samples * frameLength )
    dout.writeInt(0) // sample
    dout.writeInt(0) // block size (?!)
    //         sampleDataOffset = dis.getFilePointer();

    //         updateHeader( descr );
    val spec1 = spec.copy(numFrames = 0L, byteOrder = Some(byteOrder))
    new WriteHeaderResult(otherLen, commLen, spec1)
  }

  private final class WriteHeaderResult(val otherLen: Int, val commLen: Int, val spec1: AudioFileSpec)

  final private class WritableFileHeader(raf: RandomAccessFile, val spec: AudioFileSpec, otherLen: Int, commLen: Int)
    extends WritableAudioFileHeader {

    private var numFrames0 = 0L // spec0.numFrames

    @throws(classOf[IOException])
    def update(numFrames: Long): Unit = {
      if (numFrames == numFrames0) return

      val ssndLen = numFrames * ((spec.sampleFormat.bitsPerSample >> 3) * spec.numChannels) + 16
      val oldPos = raf.getFilePointer

      // FORM Chunk len
      raf.seek(4L)
      val fileLen = otherLen + commLen + ssndLen
      raf.writeInt((fileLen - 8).toInt)

      // COMM: numFrames
      raf.seek(otherLen + 10)
      raf.writeInt(numFrames.toInt)

      // SSND Chunk len
      raf.seek(otherLen + commLen + 4)
      raf.writeInt((ssndLen - 8).toInt)

      raf.seek(oldPos)
      //         spec = spec.copy( numFrames = numFrames )
      numFrames0 = numFrames
    }

    def byteOrder: ByteOrder = spec.byteOrder.get
  }

  final private class AsyncWritableFileHeader(ch: AsyncWritableByteChannel, val spec: AudioFileSpec,
                                              otherLen: Int, commLen: Int)
    extends AsyncWritableAudioFileHeader {

    private[this] val sync          = new AnyRef
    private[this] var numFramesRef  = 0L
    private[this] val bb            = ByteBuffer.allocate(4).order(byteOrder)

    def update(numFrames: Long): Future[Unit] = {
      val oldNumFr = sync.synchronized { numFramesRef }
      if (numFrames == oldNumFr) return Future.unit

      val ssndLen = numFrames * ((spec.sampleFormat.bitsPerSample >> 3) * spec.numChannels) + 16
      val oldPos  = ch.position

      import ch.executionContext

      // FORM Chunk len
      ch.position_=(4L)
      val fileLen = otherLen + commLen + ssndLen
      (bb: Buffer).clear()
      bb.putInt(0, (fileLen - 8).toInt)
      // println(s"bb.pos ${bb.position()}, rem ${bb.remaining()}")
      ch.write(bb).flatMap { _ =>

        // COMM: numFrames
        ch.position_=(otherLen + 10)
        (bb: Buffer).clear()
        bb.putInt(0, numFrames.toInt)
        ch.write(bb).flatMap { _ =>

          // SSND Chunk len
          ch.position_=(otherLen + commLen + 4)
          (bb: Buffer).clear()
          bb.putInt(0, (ssndLen - 8).toInt)
          ch.write(bb).map { _ =>
            ch.position_=(oldPos)
            sync.synchronized {
              if (numFramesRef != oldNumFr) throw new ConcurrentModificationException
//              println(s"updated to $numFrames")
              numFramesRef = numFrames
            }
            ()
          }
        }
      }
    }

    def byteOrder: ByteOrder = spec.byteOrder.get
  }
}
/*
 *  Wave64Header.java
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

package de.sciss.synth.io.impl

import java.io.{DataInput, DataInputStream, DataOutput, EOFException, IOException, RandomAccessFile}
import java.nio.ByteOrder

import de.sciss.synth.io.{AudioFileHeader, AudioFileSpec, AudioFileType, WritableAudioFileHeader}

/** The 64 bit version of Wave.
  *
  * Info: http://www.vcs.de/fileadmin/user_upload/MBS/PDF/Whitepaper/Informations_about_Sony_Wave64.pdf
  * (link broken; now: http://www.ambisonia.com/Members/mleese/sony_wave64.pdf )
  */
private[io] object Wave64Header extends AbstractRIFFHeader {

  import AbstractRIFFHeader._
  import AudioFileHeader._

  // private final val RIFF_MAGIC1a	= 0x72696666	// 'riff' ; first 4 bytes of RIFF-GUID in big endian notation
  // private final val RIFF_MAGIC1b	= 0x2E91CF11   //          next 2 shorts of RIFF-GUID in big endian notation

  private final val RIFF_MAGIC1 = 0x726966662E91CF11L   // RIFF-GUID in big endian notation
  private final val RIFF_MAGIC2 = 0xA5D628DB04C10000L   // ...

  private final val WAVE_MAGIC1 = 0x77617665F3ACD311L   // WAVE-GUID in big endian notation
  private final val WAVE_MAGIC2 = 0x8CD100C04F8EDB8AL   // ...

  // chunk identifiers
  private final val FMT_MAGIC1  = 0x666D7420F3ACD311L   // 'fmt ' GUID in big endian notation
  private final val FMT_MAGIC2  = 0x8CD100C04F8EDB8AL

  private final val DATA_MAGIC1 = 0x64617461F3ACD311L   // 'data' GUID in big endian notation
  private final val DATA_MAGIC2 = 0x8CD100C04F8EDB8AL

  private final val FACT_MAGIC1 = 0x66616374F3ACD311L   // 'fact' GUID in big endian notation
  private final val FACT_MAGIC2 = 0x8CD100C04F8EDB8AL

  @throws(classOf[IOException])
  def identify(dis: DataInputStream): Boolean = dis.readLong() == RIFF_MAGIC1 && dis.readLong() == RIFF_MAGIC2 && {
    dis.skipBytes(8) // length
    dis.readLong() == WAVE_MAGIC1 && dis.readLong() == WAVE_MAGIC2
  }

  @throws(classOf[IOException])
  protected def readDataInput(din: DataInput): AudioFileHeader = {
    if (din.readLong() != RIFF_MAGIC1 || din.readLong() != RIFF_MAGIC2) formatError() // RIFF
    din.skipBytes(8) // len = readLittleLong
    if (din.readLong() != WAVE_MAGIC1 || din.readLong() != WAVE_MAGIC2) formatError() // WAVE

    var chunkRem = 0L
    var fc: FormatChunk = null

    try {
      while (true) {
        while (chunkRem > 0) {
          val skp = math.min(chunkRem, 0x7FFFFFFF).toInt
          din.skipBytes(skp)
          chunkRem -= skp
        }

        val magic1    = din.readLong()
        val magic2    = din.readLong()
        val chunkLen  = readLittleLong(din) - 24  // minus 16-bytes magic and 8-bytes length
        chunkRem      = (chunkLen + 7) & 0xFFFFFFFFFFFFFFF8L

        if (magic1 == FMT_MAGIC1 && magic2 == FMT_MAGIC2) {
          fc = readFormatChunk(din, chunkRem.toInt)
          chunkRem = fc.chunkSkip

        } else if (magic1 == DATA_MAGIC1 && magic2 == DATA_MAGIC2) {
          return createReader(fc, AudioFileType.Wave64, chunkLen)

          // } else if( magic1 == MARKER_MAGIC1 && magic2 == MARKER_MAGIC2 ) {
          // markersOffset			= raf.getFilePointer()
        }
      }
    } catch {
      case _: EOFException =>
    }
    throw new IOException(s"${AudioFileType.Wave64.name} header misses data chunk")
  }

  final protected def createWriter(raf: RandomAccessFile, spec: AudioFileSpec, factSmpNumOffset: Long,
                                   dataChunkLenOff: Long): WritableAudioFileHeader =
    new WritableFileHeader(raf, spec, factSmpNumOffset = factSmpNumOffset, dataChunkLenOff = dataChunkLenOff)

  final protected def writeRiffMagic(dout: DataOutput, fileSize: Long): Unit = {
    dout.writeLong(RIFF_MAGIC1)
    dout.writeLong(RIFF_MAGIC2)
    dout.writeLong(fileSize) // total size (including cookie chunk header!)
    dout.writeLong(WAVE_MAGIC1)
    dout.writeLong(WAVE_MAGIC2)
  }

  final protected def writeFmtMagic(dout: DataOutput, fmtChunkSize: Int): Unit = {
    dout.writeLong(FMT_MAGIC1)
    dout.writeLong(FMT_MAGIC2)
    writeLittleLong(dout, fmtChunkSize)
  }

  final protected def writeFactChunk(dout: DataOutput, numFrames: Long): Unit = {
    dout.writeLong(FACT_MAGIC1)
    dout.writeLong(FACT_MAGIC2)
    writeLittleLong(dout, 32)
    dout.writeLong(numFrames)
  }

  final protected def writeDataMagic(dout: DataOutput, dataChunkSize: Long): Unit = {
    dout.writeLong(DATA_MAGIC1)
    dout.writeLong(DATA_MAGIC2)
    writeLittleLong(dout, dataChunkSize)
  }

  final protected val cookieSize    = 16
  final protected val chunkLenSize  =  8
  final protected val chunkPad      =  8

  final private class WritableFileHeader(raf: RandomAccessFile, val spec: AudioFileSpec,
                                         factSmpNumOffset: Long, dataChunkLenOff: Long)
    extends WritableAudioFileHeader {

    private var numFrames0 = 0L

    @throws(classOf[IOException])
    def update(numFrames: Long): Unit = {
      if (numFrames == numFrames0) return

      val oldPos    = raf.getFilePointer
      val fileSize  = raf.length()
      raf.seek(cookieSize)
      writeLittleLong(raf, fileSize)

      if (factSmpNumOffset != 0L) {
        raf.seek(factSmpNumOffset)
        writeLittleLong(raf, numFrames)
      }

      raf.seek(dataChunkLenOff)
      val dataChunkSize = fileSize - (dataChunkLenOff - cookieSize)
      writeLittleLong(raf, dataChunkSize)

      raf.seek(oldPos)
      numFrames0 = numFrames
    }

    def byteOrder: ByteOrder = spec.byteOrder.get
  }
}
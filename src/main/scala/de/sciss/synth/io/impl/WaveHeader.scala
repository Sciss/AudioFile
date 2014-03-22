/*
 *  WaveHeader.java
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.io
package impl

import java.io.{DataOutput, DataOutputStream, DataInput, DataInputStream, EOFException, IOException, RandomAccessFile}
import java.nio.ByteOrder
import annotation.switch

private[impl] object AbstractRIFFHeader {

  private[impl] final case class FormatChunk(sampleFormat: SampleFormat, numChannels: Int, sampleRate: Int,
                                             bytesPerFrame: Int, chunkSkip: Int) {
    def toSpec(tpe: AudioFileType, numFrames: Long): AudioFileSpec =
      AudioFileSpec(tpe, sampleFormat, numChannels = numChannels, sampleRate = sampleRate.toDouble,
        byteOrder = Some(ByteOrder.LITTLE_ENDIAN), numFrames = numFrames)
  }
}

private[impl] trait AbstractRIFFHeader extends BasicHeader {

  import AudioFileHeader._
  import AbstractRIFFHeader._

  protected final val ADTL_MAGIC    = 0x6164746C  // 'adtl'
  protected final val LABL_MAGIC    = 0x6C61626C  // 'labl'
  protected final val LTXT_MAGIC    = 0x6C747874  // 'ltxt'

  // ltxt purpose for regions
  protected final val RGN_MAGIC     = 0x72676E20  // 'rgn '

  // fmt format-code
  protected final val FORMAT_PCM    = 0x0001
  protected final val FORMAT_FLOAT  = 0x0003
  protected final val FORMAT_EXT    = 0xFFFE

  final protected def createReader(fc: FormatChunk, tpe: AudioFileType, chunkLen: Long): ReadableAudioFileHeader = {
    if (fc == null) throw new IOException(tpe.name + " header misses fmt chunk")
    val numFrames = chunkLen / fc.bytesPerFrame
    val spec      = fc.toSpec(tpe, numFrames)
    new ReadableAudioFileHeader(spec, ByteOrder.LITTLE_ENDIAN)
  }

  final protected def readFormatChunk(din: DataInput, chunkLen: Int): FormatChunk = {
    val form          = readLittleUShort(din) // format
    val numChannels   = readLittleUShort(din) // # of channels
    val sampleRate    = readLittleInt   (din) // sample rate (integer)
    val bps           = readLittleInt   (din) // bytes per frame and second (= #chan * #bits/8* rate)
    val bytesPerFrame = readLittleUShort(din) // bytes per frame (= #chan * #bits/8)
    val bitsPerSample = readLittleUShort(din) // # of bits per sample
    if (((bitsPerSample & 0x07) != 0) ||
        ((bitsPerSample >> 3) * numChannels != bytesPerFrame) ||
        ((bitsPerSample >> 3) * numChannels * sampleRate != bps)) encodingError()

    val unsignedPCM = bytesPerFrame == numChannels

    var chunkSkip = chunkLen - 16

    val isPCM = (form: @switch) match {
      case FORMAT_PCM   => true
      case FORMAT_FLOAT => false
      case FORMAT_EXT   =>
        if (chunkSkip < 24) incompleteError()
        val i1 = readLittleUShort(din)  // extension size
        if (i1 < 22) incompleteError()
        val i2 = readLittleUShort(din)  // #valid bits per sample
        din.readInt()                   // channel mask, ignore
        val i3 = readLittleUShort(din)  // GUID first two bytes
        if ((i2 != bitsPerSample) ||
           ((i3 != FORMAT_PCM) &&
            (i3 != FORMAT_FLOAT))) encodingError()
        chunkSkip -= 10
        i3 == FORMAT_PCM

      case _ => encodingError()
    }

    val sampleFormat = if (isPCM) {
      (bitsPerSample: @switch) match {
        case  8 => assert(unsignedPCM)
                   SampleFormat.UInt8   // else SampleFormat.Int8
        case 16 => SampleFormat.Int16
        case 24 => SampleFormat.Int24
        case 32 => SampleFormat.Int32
        case _  => encodingError()
      }
    } else {
      (bitsPerSample: @switch) match {
        case 32 => SampleFormat.Float
        case 64 => SampleFormat.Double
        case _  => encodingError()
      }
    }

    FormatChunk(sampleFormat, numChannels = numChannels, sampleRate = sampleRate,
      bytesPerFrame = bytesPerFrame, chunkSkip = chunkSkip)
  }

  final protected def fixOutputSpec(spec: AudioFileSpec): AudioFileSpec = {
    if (spec.sampleFormat == SampleFormat.Int8) encodingError()
    spec.byteOrder match {
      case Some(ByteOrder.LITTLE_ENDIAN)  => spec
      case None                           => spec.copy(byteOrder = Some(ByteOrder.LITTLE_ENDIAN))
      case Some(other)                    => throw new IOException(s"Unsupported byte order $other")
    }
  }

  @throws(classOf[IOException])
  final def write(dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader = {
    val spec1 = fixOutputSpec(spec)
    writeDataOutput(dos, spec1, writeSize = true)
    new NonUpdatingWritableHeader(spec1)
  }

  @throws(classOf[IOException])
  final def write(raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = {
    val spec1 = fixOutputSpec(spec)
    val res   = writeDataOutput(raf, spec1, writeSize = false)
    import res._
    createWriter(raf, spec1, factSmpNumOffset = factSmpNumOffset, dataChunkLenOff = dataChunkLenOff)
  }

  protected def createWriter(raf: RandomAccessFile, spec: AudioFileSpec, factSmpNumOffset: Long,
                             dataChunkLenOff: Long): WritableAudioFileHeader

  // The size field between the RIFF- and the WAVE-GUID (starting at byte offset 16 in the file) specifies the
  // total size of the file including the header itself. In contrast, for RIFF/WAV files, the size field at
  // offset 4 does not include the RIFF-FOURCC and the size field itself, making it 8 bytes less than the total
  // file size.

  protected def writeRiffMagic(dout: DataOutput, fileSize     : Long): Unit
  protected def writeFmtMagic (dout: DataOutput, fmtChunkSize : Int ): Unit
  protected def writeFactChunk(dout: DataOutput, numFrames    : Long): Unit
  protected def writeDataMagic(dout: DataOutput, dataChunkSize: Long): Unit

  protected def cookieSize   : Int
  protected def chunkLenSize : Int
  protected def chunkPad     : Int

  private final class HeaWriteHeaderResult(val factSmpNumOffset: Long, val dataChunkLenOff: Long)

  @throws(classOf[IOException])
  private def writeDataOutput(dout: DataOutput, spec: AudioFileSpec, writeSize: Boolean): HeaWriteHeaderResult = {
    // floating point requires FACT extension
    val isFloat         = spec.sampleFormat == SampleFormat.Float || spec.sampleFormat == SampleFormat.Double
    val fmtChunkSize    = cookieSize + chunkLenSize + (if (isFloat) 18 else 16) // FORMAT_FLOAT has extension of size 2
    val factChunkSize   = if (isFloat) cookieSize + (chunkLenSize << 1) else 0
    val preSize         = (cookieSize << 1) + chunkLenSize
    val fmtChunkStop    = preSize + fmtChunkSize
    val factChunkOff    = (fmtChunkStop + chunkPad - 1) & -chunkPad

    // (fact chunk is always automatically padded)
    val dataChunkOff    = factChunkOff + factChunkSize // (factChunkStop + chunkPad - 1) & -chunkPad
    val dataChunkLenOff = dataChunkOff + cookieSize

    val bitsPerSample   = spec.sampleFormat.bitsPerSample
    val frameSize       = (bitsPerSample >> 3) * spec.numChannels
    val numFrames       = if (writeSize) spec.numFrames else 0L
    val fileSize        = dataChunkLenOff + chunkLenSize + (numFrames * frameSize)

    writeRiffMagic(dout, fileSize)

    // fmt Chunk
    //      dout.writeInt( FMT_MAGIC )
    //      writeLittleInt( dout, fmtChunkSize - 8 )
    writeFmtMagic   (dout, fmtChunkSize)
    writeLittleShort(dout, if (isFloat) FORMAT_FLOAT else FORMAT_PCM)
    writeLittleShort(dout, spec.numChannels)
    val intRate = (spec.sampleRate + 0.5).toInt
    writeLittleInt  (dout, intRate)
    writeLittleInt  (dout, intRate * frameSize)
    writeLittleShort(dout, frameSize)
    writeLittleShort(dout, bitsPerSample)
    if (isFloat) dout.writeShort(0)
    if (factChunkOff > fmtChunkStop) {
      // padding
      var i = factChunkOff - fmtChunkStop
      while (i > 0) {
        dout.writeShort(0)
        i -= 2
      }
    }

    // fact Chunk
    val factSmpNumOffset = if (isFloat) {
      writeFactChunk(dout, numFrames)
      factChunkOff + cookieSize + chunkLenSize
    } else 0L

    // data Chunk (Header)
    val dataChunkSize = fileSize - dataChunkOff
    writeDataMagic(dout, dataChunkSize)

    new HeaWriteHeaderResult(factSmpNumOffset, dataChunkLenOff)
  }
}

private[io] object WaveHeader extends AbstractRIFFHeader {

  import AudioFileHeader._
  import AbstractRIFFHeader._

  private final val RIFF_MAGIC  = 0x52494646  // 'RIFF'
  private final val WAVE_MAGIC  = 0x57415645  // 'WAVE' (off 8)

  // chunk identifiers
  private final val FMT_MAGIC   = 0x666D7420  // 'fmt '
  private final val FACT_MAGIC  = 0x66616374  // 'fact'
  private final val DATA_MAGIC  = 0x64617461  // 'data'
  //   private final val CUE_MAGIC		= 0x63756520	// 'cue '
  //   private final val SMPL_MAGIC		= 0x73616D6C	// 'smpl'
  //   private final val INST_MAGIC		= 0x696E7374	// 'inst'

  // embedded LIST (peak speak) / list (rest of the universe speak) format
  //   private final val LIST_MAGIC		= 0x6C697374	// 'list'
  //   private final val LIST_MAGIC2	   = 0x4C495354	// 'LIST'

  //   private final val riffLengthOffset  = 4L

  @throws(classOf[IOException])
  def identify(dis: DataInputStream): Boolean = dis.readInt() == RIFF_MAGIC && {
    dis.readInt()
    dis.readInt() == WAVE_MAGIC
  }

  @throws(classOf[IOException])
  protected def readDataInput(din: DataInput): AudioFileHeader = {
    if (din.readInt() != RIFF_MAGIC) formatError() // RIFF
    din.readInt()
    //         len	= raf.len() - 8;
    if (din.readInt() != WAVE_MAGIC) formatError() // WAVE
    //         len	   -= 4;

    var chunkLen = 0
    var fc: FormatChunk = null

    try {
      while (true) {
        if (chunkLen > 0) din.skipBytes(chunkLen) // skip remainder from previous chunk

        val magic = din.readInt()
        chunkLen = (readLittleInt(din) + 1) & 0xFFFFFFFE
        // len		   -= chunkLen + 8;

        (magic: @switch) match {
          case FMT_MAGIC =>
            fc = readFormatChunk(din, chunkLen)
            chunkLen = fc.chunkSkip

          case DATA_MAGIC =>
            return createReader(fc, AudioFileType.Wave, chunkLen)

          case _ => // ignore unknown chunks
        }
      }
    } catch {
      case e: EOFException =>
    }
    throw new IOException(s"${AudioFileType.Wave.name} header misses data chunk")
  }

  final protected def createWriter(raf: RandomAccessFile, spec: AudioFileSpec, factSmpNumOffset: Long,
                                   dataChunkLenOff: Long): WritableAudioFileHeader =
    new WritableFileHeader(raf, spec, factSmpNumOffset = factSmpNumOffset, dataChunkLenOff = dataChunkLenOff)

  final protected def writeRiffMagic(dout: DataOutput, fileSize: Long): Unit = {
    dout.writeInt(RIFF_MAGIC)
    writeLittleInt(dout, (fileSize - 8).toInt) // length except RIFF-Header (file length minus 8)
    dout.writeInt(WAVE_MAGIC)
  }

  final protected def writeFmtMagic(dout: DataOutput, fmtChunkSize: Int): Unit = {
    dout.writeInt(FMT_MAGIC)
    writeLittleInt(dout, fmtChunkSize - 8)
  }

  final protected def writeFactChunk(dout: DataOutput, numFrames: Long): Unit = {
    dout.writeInt(FACT_MAGIC)
    writeLittleInt(dout, 4)
    // "With no mention of the number of channels in this computation,
    // this implies that dwSampleLength is the number of samples per channel."
    writeLittleInt(dout, numFrames.toInt)
  }

  final protected def writeDataMagic(dout: DataOutput, dataChunkSize: Long): Unit = {
    dout.writeInt(DATA_MAGIC)
    writeLittleInt(dout, (dataChunkSize - 8).toInt) // data Chunk len
  }

  final protected val cookieSize  : Int = 4
  final protected val chunkLenSize: Int = 4
  final protected val chunkPad    : Int = 2

  final private class WritableFileHeader(raf: RandomAccessFile, val spec: AudioFileSpec,
                                         factSmpNumOffset: Long, dataChunkLenOff: Long)
    extends WritableAudioFileHeader {

    private var numFrames0 = 0L

    @throws(classOf[IOException])
    def update(numFrames: Long): Unit = {
      if (numFrames == numFrames0) return

      //         val dataSize   = numFrames * ((spec.sampleFormat.bitsPerSample >> 3) * spec.numChannels)
      val oldPos    = raf.getFilePointer
      val fileSize  = raf.length()
      raf.seek(cookieSize)
      writeLittleInt(raf, (fileSize - (cookieSize + chunkLenSize)).toInt)

      if (factSmpNumOffset != 0L) {
        raf.seek(factSmpNumOffset)
        //println( "factSmpNumOffset " + factSmpNumOffset )
        // "With no mention of the number of channels in this computation,
        // this implies that dwSampleLength is the number of samples per channel."
        writeLittleInt(raf, numFrames.toInt)
      }

      raf.seek(dataChunkLenOff)
      //println( "dataChunkLenOff " + dataChunkLenOff )
      val dataChunkSize = fileSize - (dataChunkLenOff + chunkLenSize)
      writeLittleInt(raf, dataChunkSize.toInt) // data Chunk len

      raf.seek(oldPos)
      numFrames0 = numFrames
    }

    def byteOrder: ByteOrder = spec.byteOrder.get
  }
}
/*
 *  AudioFile.scala
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

import java.io.{BufferedOutputStream, ByteArrayInputStream, DataInputStream, DataOutputStream, IOException, InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousChannel, Channels}
import java.util.concurrent.atomic.AtomicLong

import de.sciss.synth.io.AudioFileHeader.opNotSupported

import scala.concurrent.{ExecutionContext, Future}
import scala.math.{max, min}

/** The <code>AudioFile</code> allows reading and writing
  * of sound files. It can operate both on a <code>RandomAccessFile</code>
  * created from a <code>File</code> instance, or on
  * an kind of <code>InputStream</code> (not every codec will
  * support this though, and functionality might be limited, for example
  * seeking is not possible with a plain <code>InputStream</code>).
  *
  * The codecs are registered with <code>AudioFileType</code>.
  * The codecs that come with AudioFile are found in the <code>impl</code>
  * package.
  *
  * Reading and writing data requires a user-buffer which holds de-interleaved
  * floating point data, that is a two dimensional <code>Array</code> which
  * holds <code>Float</code> data. A type alias <code>Frames</code> is provided
  * for this, and two helper methods <code>buffer</code>: one static to
  * construct an arbitrary user-buffer, one in class <code>AudioFile</code>
  * which creates a buffer with the appropriate channel number.
  *
  * @see    AudioFileType
  *
  * @todo   the copyTo method uses a user-buffer. it should
  *         check for the possibility to directly transfer data
  *         if input and output are compatible.
  */
object AudioFile extends ReaderFactory with AudioFilePlatform {
  // ---- synchronous ----

  @throws(classOf[IOException])
  def openRead(is: InputStream): AudioFile = {
    val dis   = dataInput(is)
    val hr    = createHeaderReader(dis)
    finishOpenStreamRead(dis, hr)
  }

  @throws(classOf[IOException])
  def openWrite(os: OutputStream, spec: AudioFileSpec): AudioFile = {
    val hw  = createHeaderWriter(spec)
    val dos = dataOutput(os)
    val afh = hw.write(dos, spec)
    val buf = createBuffer(afh)
    val sf  = spec.sampleFormat
    val bw  = sf.writerFactory.map(_.apply(Channels.newChannel(dos), buf, spec.numChannels))
      .getOrElse(noEncoder(sf))
    new WritableStreamImpl(dos, afh, bw)
  }

  /** Note that this method advances in
    * the provided input stream, its
    * previous position is not reset.
    */
  @throws(classOf[IOException])
  def readSpec(dis: DataInputStream): AudioFileSpec = {
    val hr = createHeaderReader(dis)
    hr.read(dis).spec
  }

  @throws(classOf[IOException])
  def identify(dis: DataInputStream): Option[AudioFileType.CanIdentify] =
    AudioFileType.known.find { f =>
      dis.mark(1024)
      try {
        f.identify(dis)
      } catch {
        case _: IOException => false
      } finally {
        dis.reset()
      }
    }

  // ---- asynchronous ----

  @throws(classOf[IOException])
  def openReadAsync(ch: AsyncReadableByteChannel)(implicit ec: ExecutionContext): Future[AsyncAudioFile] = {
    val hrFut = createHeaderReaderAsync(ch)
    hrFut.flatMap { hr =>
      finishOpenStreamReadAsync(ch, hr, "<stream>")
    }
  }

  // ---- generic ----

  def buffer(numChannels: Int, bufFrames: Int = 8192): Frames =
    Array.ofDim[Float](numChannels, bufFrames)

  // ---- impl  ----

  private final val useDirect = sys.props.getOrElse("de.sciss.synth.io.AudioFile.DirectMemory", "false").toBoolean

  private[io] def createBuffer(afh: AudioFileHeader): ByteBuffer = {
    val spec      = afh.spec
    val frameSize = (spec.sampleFormat.bitsPerSample >> 3) * spec.numChannels
    val bufFrames = max(1, 65536 / max(1, frameSize))
    val bufSize   = bufFrames * frameSize
    val byteBuf   = if (useDirect) ByteBuffer.allocateDirect(bufSize) else ByteBuffer.allocate(bufSize)
    byteBuf.order(afh.byteOrder)
  }

  private[io] def noDecoder(msg: AnyRef) = throw noDecoderE(msg)
  private[io] def noEncoder(msg: AnyRef) = throw noEncoderE(msg)

  private[io] def noDecoderE(msg: AnyRef) = new IOException(s"No decoder for $msg")
  private[io] def noEncoderE(msg: AnyRef) = new IOException(s"No encoder for $msg")

  // ---- synchronous impl  ----

  private[io] def openStreamWithReader(is: InputStream, reader: AudioFileType.CanRead): AudioFile = {
    val dis = dataInput(is)
    finishOpenStreamRead(dis, reader)
  }

  private def finishOpenStreamRead(dis: DataInputStream, hr: AudioFileType.CanRead): AudioFile = {
    val afh   = hr.read(dis)
    val buf   = createBuffer(afh)
    val spec  = afh.spec
    val sf    = spec.sampleFormat
    val br    = sf.readerFactory.map(_.apply(Channels.newChannel(dis), buf, spec.numChannels))
      .getOrElse(noDecoder(sf))
    new ReadableStreamImpl(dis, afh, br)
  }

  @throws(classOf[IOException])
  private[io] def createHeaderReader(dis: DataInputStream): AudioFileType.CanRead = {
    val fileType = identify(dis).getOrElse(throw new IOException("Unrecognized audio file format"))
    fileType match {
      case cr: AudioFileType.CanRead  => cr
      case _                          => noDecoder(fileType)
    }
  }

  private def dataInput (is: InputStream ) = new DataInputStream (is)
  private def dataOutput(os: OutputStream) = new DataOutputStream(new BufferedOutputStream(os, 1024))

  private[io] def createHeaderWriter(spec: AudioFileSpec): AudioFileType.CanWrite =
    spec.fileType match {
      case cw: AudioFileType.CanWrite => cw
      case other                      => noEncoder(other)
    }

  private[io] trait Basic extends AudioFile {
    protected final var framePositionVar: Long = 0L

    protected def afh: AudioFileHeader
    protected def bh : BufferHandler

    final def position: Long = framePositionVar

    def spec: AudioFileSpec = afh.spec

    @throws(classOf[IOException])
    final def copyTo(target: AudioFile, len: Long): AudioFile = {
      val tempBufSize = min(len, 8192).toInt
      val tempBuf     = Array.ofDim[Float](spec.numChannels, tempBufSize)
      var remaining   = len

      while (remaining > 0) {
        val chunkLen = min(remaining, tempBufSize).toInt
        read(tempBuf, 0, chunkLen)
        target.write(tempBuf, 0, chunkLen)
        remaining -= chunkLen
      }
      this
    }

    override def toString: String = {
      val s           = spec.toString
      val specString  = s.substring(14)
      s"AudioFile@$accessString($sourceString,$specString)"
    }

    protected def accessString: String
    protected def sourceString: String

    final def cleanUp(): Unit =
      try {
        close()
      } catch {
        case _: IOException =>
      }
  }

  private[io] trait Readable extends Basic {
    final def isReadable = true

    protected def bh: BufferReader

    override def numFrames: Long = spec.numFrames

    @throws(classOf[IOException])
    final def read(data: Frames, off: Int, len: Int): AudioFile = {
      bh.read(data, off, len)
      framePositionVar += len
      this
    }
  }

  private[io] trait ReadOnly extends Readable {
    final def isWritable = false

    @throws(classOf[IOException])
    final def flush(): AudioFile = opNotSupported

    @throws(classOf[IOException])
    final def write(data: Frames, off: Int, len: Int): AudioFile = opNotSupported

    protected final def accessString = "r"
  }

  private[io] trait Writable extends Basic {
    final def isWritable = true

    protected def bh : BufferWriter
    protected def afh: WritableAudioFileHeader

    protected final var numFramesVar: Long = 0L

    override /*final*/ def numFrames: Long = numFramesVar

    override final def spec: AudioFileSpec = afh.spec.copy(numFrames = numFramesVar)

    @throws(classOf[IOException])
    final def write(data: Frames, off: Int, len: Int): AudioFile = {
      bh.write(data, off, len)
      framePositionVar += len
      if (framePositionVar > numFramesVar) numFramesVar = framePositionVar
      this
    }

    @throws(classOf[IOException])
    final def flush(): AudioFile = {
      afh.update(numFrames)
      this
    }
  }

  private[io] trait Bidi extends Readable with Writable {
    override protected def bh: BufferBidi

    protected final def accessString = "rw"
  }

  private[io] trait WriteOnly extends Writable {
    final def isReadable = false

    protected final def accessString = "w"

    @throws(classOf[IOException])
    final def read(data: Frames, off: Int, len: Int): AudioFile = opNotSupported
  }

  private[io] trait StreamLike extends Basic {
    @throws(classOf[IOException])
    final def seek(frame: Long): AudioFile = opNotSupported

    protected final def sourceString = "<stream>"
  }

  private trait ReadOnlyStreamLike extends StreamLike with ReadOnly {
    protected def dis: DataInputStream

    private var closed: Boolean = false

    @throws(classOf[IOException])
    final def close(): Unit = {
      closed = true
      dis.close()
    }

    final def isOpen: Boolean = closed
  }

  private[io] trait WriteOnlyStreamLike extends StreamLike with WriteOnly {
    protected def dos: DataOutputStream

    private var closed: Boolean = false

    @throws(classOf[IOException])
    final def close(): Unit = {
      closed = true
      try {
        flush()
      } finally {
        dos.close()
      }
    }

    final def isOpen: Boolean = closed
  }

  private final class ReadableStreamImpl(protected val dis: DataInputStream,
                                         protected val afh: AudioFileHeader,
                                         protected val bh : BufferReader,
                                        )
    extends ReadOnlyStreamLike

  private final class WritableStreamImpl(protected val dos: DataOutputStream,
                                         protected val afh: WritableAudioFileHeader,
                                         protected val bh : BufferWriter,
                                        )
    extends WriteOnlyStreamLike


  // ---- asynchronous impl ----

  private[io] def createHeaderReaderAsync(ch: AsyncReadableByteChannel)
                                         (implicit ec: ExecutionContext): Future[AudioFileType.CanRead] = {
    val mark  = ch.position
    val arr   = new Array[Byte](1024)
    val bb    = ByteBuffer.wrap(arr)
    val fut0  = ch.read(bb)
    val fut   = fut0.andThen { case _ => ch.position = mark }
    fut.map { len =>
      val dis   = new DataInputStream(new ByteArrayInputStream(arr, 0, len))
      val fileType = identify(dis).getOrElse(throw new IOException("Unrecognized audio file format"))
      fileType match {
        case cr: AudioFileType.CanRead  => cr
        case _                          => noDecoder(fileType)
      }
    }
  }

  private[io] def finishOpenStreamReadAsync(ch: AsyncReadableByteChannel, hr: AudioFileType.CanRead,
                                            sourceString: String)
                                           (implicit ec: ExecutionContext): Future[AsyncAudioFile] = {
    val afhFut = hr.readAsync(ch)
    afhFut.map { afh =>
      val buf   = createBuffer(afh)
      val spec  = afh.spec
      // println(spec)
      val sf    = spec.sampleFormat
      val br    = sf.asyncReaderFactory.map(_.apply(ch, buf, spec.numChannels))
        .getOrElse(noDecoder(sf))
      new AsyncReadableImpl(ch, afh, br, sourceString)
    }
  }

  private[io] trait AsyncBasic extends AsyncAudioFile {
    protected final val framePositionRef = new AtomicLong(0L)

    protected def afh: AudioFileHeader
    protected def bh : AsyncBufferHandler
    protected def ch : AsynchronousChannel

    final def position: Long = framePositionRef.get()

    def spec: AudioFileSpec = afh.spec

    override def toString: String = {
      val s           = spec.toString
      val specString  = s.substring(14)
      s"AudioFile@$accessString($sourceString,$specString)"
    }

    protected def accessString: String
    protected def sourceString: String

    final def isOpen: Boolean = ch.isOpen

    def close(): Unit = ch.close()

    final def cleanUp(): Unit =
      try {
        close()
      } catch {
        case _: IOException =>
      }
  }

  private final class AsyncReadableImpl(protected val ch  : AsyncReadableByteChannel,
                                        protected val afh : AudioFileHeader,
                                        protected val bh  : AsyncBufferReader,
                                        protected val sourceString: String,
                                       )
    extends AsyncBasic {

    final def isReadable = true
    final def isWritable = false

    override def numFrames: Long = spec.numFrames

    protected final def accessString = "r-async"

    @throws(classOf[IOException])
    final def read(data: Frames, off: Int, len: Int): Future[Unit] = {
      import bh.reader.executionContext
      bh.read(data, off, len).andThen { case _ =>
        framePositionRef.addAndGet(len)
        ()
      }
    }
  }
}

trait AudioFile extends AudioFileBase {
  /** Reads sample frames from the current position
    *
    * @param  data	buffer to hold the frames reader from hard-disc.
    *               the samples will be de-interleaved such that
    *               data[0][] holds the first channel, data[1][]
    *               holds the second channel etc.
    *               ; it is allowed to have null arrays in the data
    *               (e.g. data[0] == null), in which case these channels
    *               are skipped when reading
    * @param  off  off in the buffer in sample frames, such
    *              that he first frame of the first channel will
    *              be placed in data[0][off] etc.
    * @param  len  number of continuous frames to reader.
    *
    * @throws java.io.IOException if a reader error or end-of-file occurs.
    */
  @throws(classOf[IOException])
  def read(data: Frames, off: Int, len: Int): AudioFile

  @throws(classOf[IOException])
  final def read(data: Frames): AudioFile = {
    var ch  = 0
    var num = 0
    while (ch < data.length) {
      val cd = data(ch)
      if (cd != null) {
        num = cd.length
        ch  = data.length
      } else {
        ch += 1
      }
    }
    read(data, 0, num)
  }

  /** Moves the file pointer to a specific
    * frame.
    *
    * @param  frame the sample frame which should be
    *               the new file position. this is really
    *               the sample index and not the physical file pointer.
    * @throws java.io.IOException when a seek error occurs or you try to
    *                     seek past the file's end.
    */
  @throws(classOf[IOException])
  def seek(frame: Long): AudioFile

  /** Flushes pending buffer content, and
    * updates the sound file header information
    * (i.e. numFrames fields). Usually you
    * will not have to call this method directly,
    * unless you pause writing for some time
    * and want the file information to appear
    * as accurate as possible.
    */
  def flush(): AudioFile

  /** Returns the current file pointer in sample frames
    *
    * @return		the sample frame index which is the off
    *            for the next reader or writer operation.
    *
    * @throws java.io.IOException		when the position cannot be queried
    */
  def position: Long

  @throws(classOf[IOException])
  final def position_=(frame: Long): Unit = seek(frame)

  /** Writes sample frames to the file starting at the current position.
    *
    * @param  data	buffer holding the frames to writer to hard-disc.
    *               the samples must be de-interleaved such that
    *               data[0][] holds the first channel, data[1][]
    *               holds the second channel etc.
    * @param  off  off in the buffer in sample frames, such
    *              that he first frame of the first channel will
    *              be reader from data[0][off] etc.
    * @param  len  number of continuous frames to writer.
    *
    * @throws java.io.IOException if a writer error occurs.
    */
  @throws(classOf[IOException])
  def write(data: Frames, off: Int, len: Int): AudioFile

  @throws(classOf[IOException])
  final def write(data: Frames): AudioFile = {
    var ch  = 0
    var num = 0
    while (ch < data.length) {
      val cd = data(ch)
      if (cd != null) {
        num = cd.length
        ch  = data.length
      } else {
        ch += 1
      }
    }
    write(data, 0, num)
  }

  /** Returns the number of frames
    * in the file.
    *
    * @return	the number of sample frames
    *          in the file. includes pending
    *          buffer content
    *
    * @throws	java.io.IOException	this is never thrown
    *                       but declared as of the <code>InterleavedStreamFile</code>
    *                       interface
    */
  def numFrames: Long

  /** Copies sample frames from a source sound file
    * to a target file (either another sound file
    * or any other class implementing the
    * <code>InterleavedStreamFile</code> interface).
    * Both files must have the same number of channels.
    *
    * @param	target	to file to copy to from this audio file
    * @param	numFrames	the number of frames to copy. Reading
    *                    and writing begins at the current positions
    *                    of both files.
    *
    * @throws	java.io.IOException	if a read or writer error occurs
    */
  @throws(classOf[IOException])
  def copyTo(target: AudioFile, numFrames: Long): AudioFile
}
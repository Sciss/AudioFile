/*
 *  AudioFile.scala
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

import java.io.{BufferedOutputStream, ByteArrayInputStream, DataInputStream, DataOutputStream, IOException, InputStream, OutputStream}
import java.net.URI
import java.nio.ByteBuffer
import java.nio.channels.{Channel, Channels}

import de.sciss.asyncfile.{AsyncFile, AsyncReadableByteChannel, AsyncWritableByteChannel}
import de.sciss.audiofile.AudioFile.Frames
import de.sciss.audiofile.AudioFileHeader.opNotSupported
import de.sciss.log.Logger

import scala.concurrent.{ExecutionContext, Future, Promise}
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
  * holds <code>Double</code> data. A type alias <code>Frames</code> is provided
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

  /** Opens an audio file for asynchronous reading.
    *
    * @param  uri  the path name of the file
    * @return a future  <code>AsyncAudioFile</code> object
    *         whose header is already parsed when the future completes, and
    *         that can be obtained through the <code>spec</code> method.
    *
    * @throws java.io.IOException if the file was not found, could not be reader
    *                     or has an unknown or unsupported format
    */
  @throws(classOf[IOException])
  def openReadAsync(uri: URI)(implicit executionContext: ExecutionContext): Future[AsyncAudioFile] =
    for {
      ch  <- AsyncFile.openRead(uri)
      hr  <- createHeaderReaderAsync(ch)
      aaf <- finishOpenStreamReadAsync(Some(uri), ch, hr)
    } yield {
      aaf
    }

  @throws(classOf[IOException])
  def openReadAsync(ch: AsyncReadableByteChannel)(implicit ec: ExecutionContext): Future[AsyncAudioFile] =
    for {
      hr  <- createHeaderReaderAsync(ch)
      aaf <- finishOpenStreamReadAsync(None, ch, hr)
    } yield {
      aaf
    }

  @throws(classOf[IOException])
  def openWriteAsync(uri: URI, spec: AudioFileSpec)
                    (implicit executionContext: ExecutionContext): Future[AsyncAudioFile] =
    for {
      ch  <- AsyncFile.openWrite(uri)
      aaf <- finishOpenStreamWriteAsync(Some(uri), ch, spec)
    } yield {
      aaf
    }

  @throws(classOf[IOException])
  def openWriteAsync(ch: AsyncWritableByteChannel, spec: AudioFileSpec)
                    (implicit ec: ExecutionContext): Future[AsyncAudioFile] =
    finishOpenStreamWriteAsync(None, ch, spec)

  @throws(classOf[IOException])
  def readSpecAsync(uri: URI)(implicit executionContext: ExecutionContext): Future[AudioFileSpec] =
    for {
      ch  <- AsyncFile.openRead(uri)
      hr  <- createHeaderReaderAsync(ch)
      afh <- hr.readAsync(ch)
    } yield {
      afh.spec
    }

  // ---- generic ----

  type Frames = Array[Array[Double]]

  def buffer(numChannels: Int, bufFrames: Int = 8192): Frames =
    Array.ofDim[Double](numChannels, bufFrames)

  /** System property key. The boolean property can be set to `true` or `false`.
    * It will only be read once, the first time a buffer is allocated. The default is `false`.
    * When `true`, `ByteBuffer.allocateDirect` is used instead of `allocate`, possibly using faster
    * direct memory.
    */
  val KEY_DIRECT_MEMORY = "AudioFile.DirectMemory"

  val log: Logger = new Logger("AudioFile")

  // ---- impl  ----

  private lazy val _useDirect = sys.props.getOrElse(KEY_DIRECT_MEMORY, "false").toBoolean

  private[audiofile] def allocByteBuffer(size: Int): ByteBuffer =
    if (_useDirect) ByteBuffer.allocateDirect(size) else ByteBuffer.allocate(size)

  private[audiofile] def createBuffer(afh: AudioFileHeader): ByteBuffer = {
    val spec      = afh.spec
    val frameSize = (spec.sampleFormat.bitsPerSample >> 3) * spec.numChannels
    val bufFrames = max(1, 65536 / max(1, frameSize))
    val bufSize   = bufFrames * frameSize
    val byteBuf   = allocByteBuffer(bufSize)
    byteBuf.order(afh.byteOrder)
  }

  private[audiofile] def noDecoder(msg: AnyRef) = throw noDecoderE(msg)
  private[audiofile] def noEncoder(msg: AnyRef) = throw noEncoderE(msg)

  private[audiofile] def noDecoderE(msg: AnyRef) = new IOException(s"No decoder for $msg")
  private[audiofile] def noEncoderE(msg: AnyRef) = new IOException(s"No encoder for $msg")

  // ---- synchronous impl  ----

  private[audiofile] def openStreamWithReader(is: InputStream, reader: AudioFileType.CanRead): AudioFile = {
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
  private[audiofile] def createHeaderReader(dis: DataInputStream): AudioFileType.CanRead = {
    val fileType = identify(dis).getOrElse(throw new IOException("Unrecognized audio file format"))
    fileType match {
      case cr: AudioFileType.CanRead  => cr
      case _                          => noDecoder(fileType)
    }
  }

  private def dataInput (is: InputStream ) = new DataInputStream (is)
  private def dataOutput(os: OutputStream) = new DataOutputStream(new BufferedOutputStream(os, 1024))

  private[audiofile] def createHeaderWriter(spec: AudioFileSpec): AudioFileType.CanWrite =
    spec.fileType match {
      case cw: AudioFileType.CanWrite => cw
      case other                      => noEncoder(other)
    }

  private[audiofile] trait Basic extends AudioFile {
    protected final var framePositionVar: Long = 0L

    protected def afh: AudioFileHeader
    protected def bh : BufferHandler

    final def position: Long = framePositionVar

    def spec: AudioFileSpec = afh.spec

    @throws(classOf[IOException])
    final def copyTo(target: AudioFile, len: Long): this.type = {
      val tempBufSize = min(len, 8192).toInt
      val tempBuf     = Array.ofDim[Double](spec.numChannels, tempBufSize)
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

  private[audiofile] trait Readable extends Basic {
    final def isReadable = true

    protected def bh: BufferReader

    override def numFrames: Long = spec.numFrames

    @throws(classOf[IOException])
    final def read(data: Frames, off: Int, len: Int): this.type = {
      bh.read(data, off, len)
      framePositionVar += len
      this
    }
  }

  private[audiofile] trait ReadOnly extends Readable {
    final def isWritable = false

    @throws(classOf[IOException])
    final def flush(): this.type = opNotSupported

    @throws(classOf[IOException])
    final def write(data: Frames, off: Int, len: Int): this.type = opNotSupported

    protected final def accessString = "r"
  }

  private[audiofile] trait Writable extends Basic {
    final def isWritable = true

    protected def bh : BufferWriter
    protected def afh: WritableAudioFileHeader

    protected final var numFramesVar: Long = 0L

    override /*final*/ def numFrames: Long = numFramesVar

    override final def spec: AudioFileSpec = afh.spec.copy(numFrames = numFramesVar)

    @throws(classOf[IOException])
    final def write(data: Frames, off: Int, len: Int): this.type = {
      bh.write(data, off, len)
      framePositionVar += len
      if (framePositionVar > numFramesVar) numFramesVar = framePositionVar
      this
    }

    @throws(classOf[IOException])
    final def flush(): this.type = {
      afh.update(numFrames)
      this
    }
  }

  private[audiofile] trait Bidi extends Readable with Writable {
    override protected def bh: BufferBidi

    protected final def accessString = "rw"
  }

  private[audiofile] trait WriteOnly extends Writable {
    final def isReadable = false

    protected final def accessString = "w"

    @throws(classOf[IOException])
    final def read(data: Frames, off: Int, len: Int): this.type = opNotSupported
  }

  private[audiofile] trait StreamLike extends Basic {

    final def seek(frame: Long): this.type = opNotSupported

    final def canSeek = false

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

  private[audiofile] trait WriteOnlyStreamLike extends StreamLike with WriteOnly {
    protected def dos: DataOutputStream

    private var closed: Boolean = false

    @throws(classOf[IOException])
    final def close(): Unit = {
      closed = true
      try {
        flush()
        ()
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
    extends ReadOnlyStreamLike {

    def uri: Option[URI] = None
  }

  private final class WritableStreamImpl(protected val dos: DataOutputStream,
                                         protected val afh: WritableAudioFileHeader,
                                         protected val bh : BufferWriter,
                                        )
    extends WriteOnlyStreamLike {

    def uri: Option[URI] = None
  }


  // ---- asynchronous impl ----

  private[audiofile] def createHeaderReaderAsync(ch: AsyncReadableByteChannel)
                                         (implicit ec: ExecutionContext): Future[AudioFileType.CanRead] = {
    val mark  = ch.position
    val arr   = new Array[Byte](128)  // XXX TODO arbitrary size; is there a format that needs more than this to identify?
    val bb    = ByteBuffer.wrap(arr)
    val fut0  = ch.read(bb)
    val fut   = fut0.andThen { case _ =>
      ch.position = mark
    }
    fut.map { len =>
      val dis   = new DataInputStream(new ByteArrayInputStream(arr, 0, len))
      val fileType = identify(dis).getOrElse(throw new IOException("Unrecognized audio file format"))
      fileType match {
        case cr: AudioFileType.CanRead  => cr
        case _                          => noDecoder(fileType)
      }
    }
  }

  private[audiofile] def finishOpenStreamReadAsync(uri: Option[URI], ch: AsyncReadableByteChannel,
                                                   hr: AudioFileType.CanRead)
                                                  (implicit ec: ExecutionContext): Future[AsyncAudioFile] = {
    val afhFut = hr.readAsync(ch)
    afhFut.map { afh =>
      val buf   = createBuffer(afh)
      val spec  = afh.spec
      // println(spec)
      val sf    = spec.sampleFormat
      val br    = sf.asyncReaderFactory.map(_.apply(ch, buf, spec.numChannels))
        .getOrElse(noDecoder(sf))
      new AsyncReadableImpl(uri, ch, afh, br)
    }
  }

  private[audiofile] def finishOpenStreamWriteAsync(uri: Option[URI], ch: AsyncWritableByteChannel,
                                                    spec: AudioFileSpec)
                                                   (implicit ec: ExecutionContext): Future[AsyncAudioFile] = {
    val hw      = createHeaderWriter(spec)
    val afhFut  = hw.writeAsync(ch, spec)
    afhFut.map { afh =>
      val buf   = createBuffer(afh)
      val sf    = spec.sampleFormat
      sf.asyncBidiFactory match {
        case Some(bbf) =>
          val bb = bbf(ch, buf, spec.numChannels)
          new AsyncBidiImpl(uri, ch, afh, bb)

        case None =>
          val bw = sf.asyncWriterFactory.map(_.apply(ch, buf, spec.numChannels))
            .getOrElse(noEncoder(sf))
          new AsyncWritableImpl(uri, ch, afh, bw)
      }
    }
  }

  private trait AsyncBasic extends AsyncAudioFile {
    protected final val sync              = new AnyRef
    protected final var framePositionRef  = 0L
    protected final var _state            = 0     // 0 - idle, 1 - read, 2 - write, 3 - closed
    protected final var _shouldClose      = false
//    protected final var _pending: Future[Unit] = null
    protected final var _dirty            = false

    protected def afh: AudioFileHeader
    protected val bh : AsyncBufferHandler
    protected def ch : AsyncReadableByteChannel

    protected def sampleDataOffset: Long

    protected val sourceString: String = uri.fold("<stream>")(_.toString)

    final def position: Long = sync.synchronized { framePositionRef }

    def spec: AudioFileSpec = afh.spec

    protected final def checkState(): Unit = _state match {
      case 0 => ()
      case 1 => throw new IOException(s"File $sourceString has a pending read" )
      case 2 => throw new IOException(s"File $sourceString has a pending write")
      case 3 => throw new IOException(s"File $sourceString was already closed" )
    }

    protected final def reachedTarget(): Unit = {
      log.debug(s"reachedTarget() ${_state} -> ${_shouldClose}")
      _state = 0
      if (_shouldClose) flushAndClose()
    }

    override def toString: String = {
      val s           = spec.toString
      val specString  = s.substring(14)
      s"AudioFile@$accessString($sourceString,$specString)"
    }

    protected def accessString: String

    final def isOpen: Boolean = sync.synchronized {
      !(_state == 3 || _shouldClose)
    }

    private[this] lazy val _closedPr = Promise[Unit]()

    private def flushAndClose(): Unit = {
      import bh.executionContext
      val fut = flush().flatMap { _ =>
        sync.synchronized {
          val res = ch.close()
          _state  = 3
          res
        }
      }
      _closedPr.completeWith(fut)
    }

    def close(): Future[Unit] = sync.synchronized {
      log.debug(s"close() state = ${_state} dirty = ${_dirty}")
      _shouldClose = true
      _state match {
        case 0 =>
          if (_dirty) {
            flushAndClose()
            _closedPr.future
          } else {
            _state = 3
            Future.unit
          }

        case 1 | 2  => _closedPr.future
        case 3      => Future.unit
      }
    }

//    def close(): Future[Unit] = sync.synchronized {
//      _targetState = 3
//      _state match {
//        case 3  => Future.unit
//        case _  =>
//          import bh.executionContext
//          flush().flatMap { _ =>
//            ch.close()
//          } .andThen { _ =>
//            _state = 3
//          }
//      }
//    }

    final def cleanUp(): Unit =
      try {
        close()
        ()
      } catch {
        case _: IOException =>
      }

    final def seek(frame: Long): this.type = sync.synchronized {
      checkState()
      val physical      = sampleDataOffset + frame * bh.frameSize
      ch.position       = physical
      framePositionRef  = frame
      this
    }

    final def canSeek = true
  }

  private trait AsyncBasicReadable extends AsyncBasic {
    protected val bh : AsyncBufferReader

    final def isReadable = true

    final def read(data: Frames, off: Int, len: Int): Future[Unit] = sync.synchronized {
      if (len <= 0) return Future.unit
      checkState()
      _state = 1

      import bh.executionContext
      val oldPos = position
      val _pending = bh.read(data, off, len).andThen { case _ =>
        sync.synchronized {
          framePositionRef = oldPos + len
          reachedTarget()
        }
      }
      _pending
    }
  }

  private trait AsyncBasicWritable extends AsyncBasic {
    protected val bh : AsyncBufferWriter

    protected def afh: AsyncWritableAudioFileHeader

    final protected var numFramesVar  : Long = 0L    // protected by `sync`
//    final protected var flushFramesRef: Long = 0L    // protected by `sync`

    final def isWritable = true

    override def numFrames: Long = sync.synchronized { numFramesVar }

    override final def spec: AudioFileSpec = afh.spec.copy(numFrames = numFramesVar)

    final def write(data: Frames, off: Int, len: Int): Future[Unit] = sync.synchronized {
      if (len <= 0) return Future.unit
      checkState()
      _state  = 2
      _dirty  = true

      import bh.executionContext
      val oldPos = position
      val newPos = oldPos + len
      val _pending = bh.write(data, off, len).andThen { case _ =>
        sync.synchronized {
          framePositionRef = newPos
          if (newPos > numFramesVar) numFramesVar = newPos
          reachedTarget()
        }
      }
      _pending
    }

    final def flush(): Future[Unit] = sync.synchronized {
      log.debug(s"flush() state = ${_state} dirty = ${_dirty}")
      if (!_dirty) return Future.unit
      checkState()

      _state = 2 // 'write'
      import bh.executionContext
      val _pending = afh.updateAsync(numFramesVar).andThen { case _ =>
        _dirty = false
        reachedTarget()
      }
      _pending
    }
  }

  private final class AsyncReadableImpl(
                                         val uri: Option[URI],
                                         protected val ch  : AsyncReadableByteChannel,
                                         protected val afh : AudioFileHeader,
                                         protected val bh  : AsyncBufferReader,
                                       )
    extends AsyncBasicReadable {

    protected final val sampleDataOffset = ch.position

    protected def accessString = "r-async"

    def isWritable = false

    override def numFrames: Long = spec.numFrames

    def write(data: Frames, off: Int, len: Int): Future[Unit] = opNotSupported

    def flush(): Future[Unit] = Future.unit
  }

  private final class AsyncWritableImpl(
                                         val uri: Option[URI],
                                         protected val ch  : AsyncWritableByteChannel,
                                         protected val afh : AsyncWritableAudioFileHeader,
                                         protected val bh  : AsyncBufferWriter,
                                       )
    extends AsyncBasicWritable {

    protected final val sampleDataOffset = ch.position

    protected def accessString = "w-async"

    def isReadable = false

    def read(data: Frames, off: Int, len: Int): Future[Unit] = opNotSupported
  }

  private final class AsyncBidiImpl(
                                     val uri: Option[URI],
                                     protected val ch  : AsyncWritableByteChannel,
                                     protected val afh : AsyncWritableAudioFileHeader,
                                     protected val bh  : AsyncBufferBidi,
                                   )
    extends AsyncBasicReadable with AsyncBasicWritable {

    protected final val sampleDataOffset = ch.position

    protected def accessString = "rw-async"
  }
}

trait AudioFile extends AudioFileBase with Channel {
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
  def read(data: Frames, off: Int, len: Int): this.type

  @throws(classOf[IOException])
  final def read(data: Frames): this.type = {
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

  /** Flushes pending buffer content, and
    * updates the sound file header information
    * (i.e. numFrames fields). Usually you
    * will not have to call this method directly,
    * unless you pause writing for some time
    * and want the file information to appear
    * as accurate as possible.
    */
  def flush(): this.type

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
  def write(data: Frames, off: Int, len: Int): this.type

  @throws(classOf[IOException])
  final def write(data: Frames): this.type = {
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
  def copyTo(target: AudioFile, numFrames: Long): this.type

  /** Flushes and closes the file
    *
    * @throws java.io.IOException if an error occurs during buffer flush
    *                     or closing the file.
    */
  @throws(classOf[IOException])
  def close(): Unit
}

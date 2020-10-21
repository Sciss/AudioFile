package de.sciss.synth.io

import java.io.{BufferedInputStream, DataInputStream, File, FileInputStream, IOException, InputStream, RandomAccessFile}
import java.nio.channels.Channels

import de.sciss.synth.io.AudioFile.{Basic, Bidi, ReadOnly, Writable, WriteOnly, createBuffer, createHeaderReader, createHeaderWriter, noDecoder, noEncoder}

/** The JVM platform supports File I/O, e.g. opening an audio file using `openRead(f: File)`. */
trait AudioFilePlatform {
  /** Returns the underlying <code>File</code>
    * provided to the <code>AudioFile</code> constructor.
    */
  trait HasFile extends AudioFile {
    def file: File
  }
  
  /** Opens an audio file for reading.
    *
    * @param  f  the path name of the file
    * @return a new <code>AudioFile</code> object
    *         whose header is already parsed and can
    *         be obtained through the <code>spec</code> method.
    *
    * @throws java.io.IOException if the file was not found, could not be reader
    *                     or has an unknown or unsupported format
    */
  @throws(classOf[IOException])
  def openRead(f: File): AudioFile.HasFile = {
    val raf   = new RandomAccessFile(f, "r")
    val dis   = bufferedDataInput(Channels.newInputStream(raf.getChannel))
    val hr    = createHeaderReader(dis)
    finishOpenFileRead(f, raf, hr)
  }

  /** Opens an audio file for reading/writing. The pathname
    * is determined by the <code>file</code> field of the provided <code>AudioFileInfo</code>.
    * If a file denoted by this path already exists, it will be
    * deleted before opening.
    * <p>
    * Note that the initial audio file header is written immediately.
    * Special tags for the header thus need to be set in the <code>AudioFileInfo</code>
    * before calling this method, including markers and regions. It is not
    * possible to writer markers and regions after the file has been opened
    * (since the header size has to be constant).
    *
    * @param  f  the path name of the file.
    * @param  spec   format and resolution of the new audio file.
    *                the header is immediately written to the hard-disc
    *
    * @throws java.io.IOException if the file could not be created or the
    *                     format is unsupported
    */
  @throws(classOf[IOException])
  def openWrite(f: File, spec: AudioFileSpec): AudioFile.HasFile = {
    val hw  = createHeaderWriter(spec)
    if (f.exists) f.delete()
    val raf = new RandomAccessFile(f, "rw")
    val afh = hw.write(raf, spec)
    val buf = createBuffer(afh)
    val sf  = spec.sampleFormat
    val ch  = raf.getChannel
    sf.bidiFactory match {
      case Some(bbf) =>
        val bb = bbf(ch, ch, buf, spec.numChannels)
        new BidiFileImpl(f, raf, afh, bb)
      case None =>
        val bw = sf.writerFactory.map(_.apply(ch, buf, spec.numChannels)).getOrElse(noEncoder(sf))
        new WritableFileImpl(f, raf, afh, bw)
    }
  }

  @throws(classOf[IOException])
  def openWrite(path: String, spec: AudioFileSpec): AudioFile.HasFile = openWrite(new File(path), spec)

  @throws(classOf[IOException])
  def readSpec(f: File): AudioFileSpec = {
    val raf = new RandomAccessFile(f, "r")
    try {
      val dis   = bufferedDataInput(Channels.newInputStream(raf.getChannel))
      val hr    = createHeaderReader(dis)
      raf.seek(0L) // BufferedInputStream did advance the position!
      hr.read(raf).spec
    } finally {
      raf.close()
    }
  }

  @throws(classOf[IOException])
  def readSpec(path: String): AudioFileSpec = readSpec(new File(path))

  /** Determines the type of audio file.
    *
    * @param		f   the pathname of the file
    * @return		the type code as defined in <code>AudioFileInfo</code>,
    *            e.g. <code>TYPE_AIFF</code>. Returns <code>TYPE_UNKNOWN</code>
    *            if the file could not be identified.
    *
    * @throws java.io.IOException if the file could not be reader
    */
  @throws(classOf[IOException])
  def identify(f: File): Option[AudioFileType.CanIdentify] = {
    val dis = bufferedDataInput(new FileInputStream(f))
    try {
      AudioFile.identify(dis)
    } finally {
      dis.close()
    }
  }

  @throws(classOf[IOException])
  def identify(path: String): Option[AudioFileType] = identify(new File(path))

  private[io] def openFileWithReader(f: File, reader: AudioFileType.CanRead): AudioFile.HasFile = {
    val raf = new RandomAccessFile(f, "r")
    finishOpenFileRead(f, raf, reader)
  }

  private def finishOpenFileRead(f: File, raf: RandomAccessFile, hr: AudioFileType.CanRead): AudioFile.HasFile = {
    raf.seek(0L) // BufferedInputStream did advance the position!
    val afh   = hr.read(raf)
    val buf   = createBuffer(afh)
    val spec  = afh.spec
    val sf    = spec.sampleFormat
    val br    = sf.readerFactory.map(_.apply(raf.getChannel, buf, spec.numChannels))
      .getOrElse(noDecoder(sf))
    new ReadableFileImpl(f, raf, afh, br)
  }

  private def bufferedDataInput(is: InputStream) = new DataInputStream(new BufferedInputStream(is, 1024))

  private trait FileLike extends Basic with AudioFile.HasFile {
    protected def raf: RandomAccessFile

    private val sampleDataOffset = raf.getFilePointer

    protected final def sourceString: String = file.toString

    final def seek(frame: Long): AudioFile = {
      val physical = sampleDataOffset + frame * bh.frameSize
      raf.seek(physical)
      framePositionVar = frame
      this
    }

    final def isOpen: Boolean = raf.getChannel.isOpen
  }

  private trait ReadOnlyFileLike extends FileLike with ReadOnly {
    final def close(): Unit = raf.close()
  }

  private trait WritableFileLike extends FileLike with Writable {
    final def close(): Unit =
      try {
        flush()
      } finally {
        raf.close()
      }
  }

  private trait WriteOnlyFileLike extends WritableFileLike with WriteOnly
  private trait BidiFileLike      extends WritableFileLike with Bidi

  private final class ReadableFileImpl(val file: File,
                                       protected val raf: RandomAccessFile,
                                       protected val afh: AudioFileHeader,
                                       protected val bh : BufferReader,
                                      )
    extends ReadOnlyFileLike

  private final class WritableFileImpl(val file: File,
                                       protected val raf: RandomAccessFile,
                                       protected val afh: WritableAudioFileHeader,
                                       protected val bh : BufferWriter,
                                      )
    extends WriteOnlyFileLike

  private final class BidiFileImpl(val file: File,
                                   protected val raf: RandomAccessFile,
                                   protected val afh: WritableAudioFileHeader,
                                   protected val bh : BufferBidi,
                                  )
    extends BidiFileLike
}

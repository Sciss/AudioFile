/*
 *  AudioFileType.scala
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

import java.io.{DataInputStream, DataOutputStream, IOException, InputStream}
import java.nio.ByteOrder

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.Future

/** A recognized audio file type. */
sealed trait AudioFileType {
  /** @return  the unique identifier of the file type */
  def id: String

  /** Human readable name of the format. */
  def name: String

  /** @return  the default extension (period not included) */
  def extension: String

  /** @return  a list of alternative extensions, including the default `extension` */
  def extensions: Vec[String]

  def supportedFormats: Vec[SampleFormat]
}

object AudioFileType extends AudioFileTypePlatform {

  sealed trait CanIdentify extends AudioFileType {
    @throws(classOf[IOException])
    private[io] def identify(dis: DataInputStream): Boolean

//    private[io] def identifyAsync(ch: AsyncReadableByteChannel): Future[Boolean]
  }

  trait CanRead extends AudioFileType with CanReadPlatform {
    @throws(classOf[IOException])
    private[io] def read(dis: DataInputStream): AudioFileHeader

    private[io] def readAsync(ch: AsyncReadableByteChannel): Future[AudioFileHeader]
  }

  trait CanWrite extends AudioFileType with CanWritePlatform {
    @throws(classOf[IOException])
    private[io] def write(dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader
  }

  //   private val sync  = new AnyRef
  private var allVar      = Vec.empty[AudioFileType]
  private var knownVar    = Vec.empty[CanIdentify]
  private var readableVar = Vec.empty[CanRead]
  private var writableVar = Vec.empty[CanWrite]

  /** Restores a file type from its identifier.
    * Throws a runtime exception if the identifier is unknown.
    */
  def apply(id: String): AudioFileType = allVar.find(_.id == id).getOrElse(
    throw new IllegalArgumentException("Unknown identifier " + id))

  private[io] def register(fileType: AudioFileType): Unit = {
    allVar :+= fileType

    fileType match {
      case ci: CanIdentify => knownVar :+= ci
      case _ =>
    }
    fileType match {
      case cr: CanRead => readableVar :+= cr
      case _ =>
    }
    fileType match {
      case cw: CanWrite => writableVar :+= cw
      case _ =>
    }
  }

  register(AIFF)
  register(Wave)
  register(Wave64)
  register(NeXT)
  register(IRCAM)
  register(Raw)

  /** Note: this Iterator does not include the Raw type
    * which usually requires special handling.
    */
  def known   : Vec[CanIdentify] = knownVar
  def readable: Vec[CanRead    ] = readableVar
  def writable: Vec[CanWrite   ] = writableVar

  /** Apple's audio interchange file format. */
  case object AIFF extends CanIdentify with CanRead with CanWrite with AIFFPlatform {

    import impl.{AIFFHeader => Impl}

    final val name        = "AIFF"
    final val id          = "aiff"
    final val extension   = "aif"
    final val extensions  = Vec("aif", "aiff", "aifc")

    def supportedFormats: Vec[SampleFormat] = SampleFormat.allSigned

    private[io] def identify(dis: DataInputStream ): Boolean          = Impl.identify(dis)
    private[io] def read    (dis: DataInputStream ): AudioFileHeader  = Impl.read(dis)

    private[io] def write   (dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader =
      Impl.write(dos, spec)

    private[io] def readAsync(ch: AsyncReadableByteChannel): Future[AudioFileHeader] =
      Impl.readAsync(ch)
  }

  /** The NeXT .snd or Sun .au format. */
  case object NeXT extends CanIdentify with CanRead with CanWrite with NeXTPlatform {

    import impl.{NeXTHeader => Impl}

    final val name        = "NeXT/Sun"
    final val id          = "next"
    final val extension   = "au"
    final val extensions  = Vec("au", "snd")

    def supportedFormats: Vec[SampleFormat] = SampleFormat.allSigned

    private[io] def identify(dis: DataInputStream ): Boolean         = Impl.identify(dis)
    private[io] def read    (dis: DataInputStream ): AudioFileHeader = Impl.read(dis)
    private[io] def write   (dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(dos, spec)

    private[io] def readAsync(ch: AsyncReadableByteChannel) = ???
  }

  /** Microsoft's Wave (RIFF) format. */
  case object Wave extends CanIdentify with CanRead with CanWrite with WavePlatform {

    import impl.{WaveHeader => Impl}

    final val name              = "WAVE"
    final val id                = "wav"
    final val extension         = "wav"
    final val extensions        = Vec("wav", "wave")
    final val supportedFormats  = SampleFormat.UInt8 +: SampleFormat.fromInt16

    private[io] def identify(dis: DataInputStream ): Boolean         = Impl.identify(dis)
    private[io] def read    (dis: DataInputStream ): AudioFileHeader = Impl.read(dis)
    private[io] def write   (dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(dos, spec)

    private[io] def readAsync(ch: AsyncReadableByteChannel) = ???
  }

  /** IRCAM, Berkeley or Carl sound format (BICSF). */
  case object IRCAM extends CanIdentify with CanRead with CanWrite with IRCAMPlatform {

    import impl.{IRCAMHeader => Impl}

    final val name        = "IRCAM"
    final val id          = "ircam"
    final val extension   = "sf"
    final val extensions  = Vec("sf", "irc")

    def supportedFormats: Vec[SampleFormat] = SampleFormat.allSigned

    private[io] def identify(dis: DataInputStream ): Boolean         = Impl.identify(dis)
    private[io] def read    (dis: DataInputStream ): AudioFileHeader = Impl.read(dis)
    private[io] def write   (dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(dos, spec)

    private[io] def readAsync(ch: AsyncReadableByteChannel) = ???
  }

  /** Raw (headerless) file type. */
  case object Raw extends CanWrite with RawPlatform {

    import impl.{RawHeader => Impl}

    final val name        = "Raw"
    final val id          = "raw"
    final val extension   = "raw"
    final val extensions  = Vec("raw")

    def supportedFormats: Vec[SampleFormat] = SampleFormat.all

    private[io] def write(dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(dos, spec)

    def reader(spec: AudioFileSpec): ReaderFactory = {
      val spec1 = if (spec.fileType == Raw) spec else spec.copy(fileType = Raw)
      Readable(spec1)
    }

    private final case class Readable(spec: AudioFileSpec) extends ReaderFactory with CanRead with ReadablePlatform {
      def id              : String            = Raw.id
      def name            : String            = Raw.name
      def extension       : String            = Raw.extension
      def extensions      : Vec[String]       = Raw.extensions
      def supportedFormats: Vec[SampleFormat] = Raw.supportedFormats

      def openRead(is: InputStream): AudioFile = AudioFile.openStreamWithReader(is, this)

      def read(dis: DataInputStream ): AudioFileHeader = reader(dis.available())

      def readAsync(ch: AsyncReadableByteChannel) = ???

      protected def reader(fileSize: Long): AudioFileHeader = {
        val bpf         = spec.numChannels * (spec.sampleFormat.bitsPerSample >> 3)
        val numFrames   = fileSize / bpf
        val byteOrder   = spec.byteOrder.getOrElse(ByteOrder.nativeOrder())
        val spec1       = spec.copy(byteOrder = Some(byteOrder),
          numFrames     = numFrames)
        ReadableAudioFileHeader(spec1, byteOrder)
      }
    }
  }

  /** Sony Wave 64, the 64-bit extension of the Wave format. */
  case object Wave64 extends CanIdentify with CanRead with CanWrite with Wave64Platform {

    import impl.{Wave64Header => Impl}

    final val name              = "Wave64"
    final val id                = "w64"
    final val extension         = "w64"
    final val extensions        = Vec("w64", "wave64")
    final val supportedFormats  = SampleFormat.UInt8 +: SampleFormat.fromInt16

    private[io] def identify(dis: DataInputStream ): Boolean         = Impl.identify(dis)
    private[io] def read    (dis: DataInputStream ): AudioFileHeader = Impl.read(dis)
    private[io] def write   (dos: DataOutputStream, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(dos, spec)

    private[io] def readAsync(ch: AsyncReadableByteChannel) = ???
  }
}
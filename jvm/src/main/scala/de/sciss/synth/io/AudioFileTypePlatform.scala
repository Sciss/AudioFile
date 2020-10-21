package de.sciss.synth.io

import java.io.{File, IOException, RandomAccessFile}

import de.sciss.synth.io.AudioFileType.CanRead

/** The JVM platform supports File I/O, e.g. reading from and writing to `RandomAccessFile`. */
trait AudioFileTypePlatform {
  trait CanReadPlatform {
    @throws(classOf[IOException])
    private[io] def read(raf: RandomAccessFile): AudioFileHeader
  }

  trait CanWritePlatform {
    @throws(classOf[IOException])
    private[io] def write(raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader
  }

  trait AIFFPlatform {
    import impl.{AIFFHeader => Impl}

    private[io] def read    (raf: RandomAccessFile)                     : AudioFileHeader         = Impl.read(raf)
    private[io] def write   (raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(raf, spec)
  }

  trait NeXTPlatform {
    import impl.{NeXTHeader => Impl}

    private[io] def read    (raf: RandomAccessFile)                     : AudioFileHeader         = Impl.read(raf)
    private[io] def write   (raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(raf, spec)
  }

  trait WavePlatform {
    import impl.{WaveHeader => Impl}

    private[io] def read    (raf: RandomAccessFile)                     : AudioFileHeader         = Impl.read(raf)
    private[io] def write   (raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(raf, spec)
  }

  trait IRCAMPlatform {
    import impl.{IRCAMHeader => Impl}

    private[io] def read    (raf: RandomAccessFile)                     : AudioFileHeader         = Impl.read(raf)
    private[io] def write   (raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(raf, spec)
  }

  trait RawPlatform {
    import impl.{RawHeader => Impl}

    private[io] def write(raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(raf, spec)
  }

  trait ReadablePlatform extends CanRead {
    protected def reader(fileSize: Long): AudioFileHeader

    def openRead(f: File): AudioFile.HasFile = AudioFile.openFileWithReader(f, this)

    def read(raf: RandomAccessFile): AudioFileHeader = reader(raf.length())
  }

  trait Wave64Platform {
    import impl.{Wave64Header => Impl}

    private[io] def read    (raf: RandomAccessFile)                     : AudioFileHeader         = Impl.read(raf)
    private[io] def write   (raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(raf, spec)
  }
}

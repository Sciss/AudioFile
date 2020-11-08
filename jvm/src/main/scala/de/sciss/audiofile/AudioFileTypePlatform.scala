/*
 *  AudioFileTypePlatform.scala
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

import java.io.{File, IOException, RandomAccessFile}

import de.sciss.audiofile.AudioFileType.CanRead

/** The JVM platform supports File I/O, e.g. reading from and writing to `RandomAccessFile`. */
trait AudioFileTypePlatform {
  trait CanReadPlatform {
    @throws(classOf[IOException])
    private[audiofile] def read(raf: RandomAccessFile): AudioFileHeader
  }

  trait CanWritePlatform {
    @throws(classOf[IOException])
    private[audiofile] def write(raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader
  }

  trait AIFFPlatform {
    import impl.{AIFFHeader => Impl}

    private[audiofile] def read    (raf: RandomAccessFile)                     : AudioFileHeader         = Impl.read(raf)
    private[audiofile] def write   (raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(raf, spec)
  }

  trait NeXTPlatform {
    import impl.{NeXTHeader => Impl}

    private[audiofile] def read    (raf: RandomAccessFile)                     : AudioFileHeader         = Impl.read(raf)
    private[audiofile] def write   (raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(raf, spec)
  }

  trait WavePlatform {
    import impl.{WaveHeader => Impl}

    private[audiofile] def read    (raf: RandomAccessFile)                     : AudioFileHeader         = Impl.read(raf)
    private[audiofile] def write   (raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(raf, spec)
  }

  trait IRCAMPlatform {
    import impl.{IRCAMHeader => Impl}

    private[audiofile] def read    (raf: RandomAccessFile)                     : AudioFileHeader         = Impl.read(raf)
    private[audiofile] def write   (raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(raf, spec)
  }

  trait RawPlatform {
    import impl.{RawHeader => Impl}

    private[audiofile] def write(raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(raf, spec)
  }

  trait ReadablePlatform extends CanRead {
    protected def reader(fileSize: Long): AudioFileHeader

    def openRead(f: File): AudioFile = AudioFile.openFileWithReader(f, this)

    def read(raf: RandomAccessFile): AudioFileHeader = reader(raf.length())
  }

  trait Wave64Platform {
    import impl.{Wave64Header => Impl}

    private[audiofile] def read    (raf: RandomAccessFile)                     : AudioFileHeader         = Impl.read(raf)
    private[audiofile] def write   (raf: RandomAccessFile, spec: AudioFileSpec): WritableAudioFileHeader = Impl.write(raf, spec)
  }
}

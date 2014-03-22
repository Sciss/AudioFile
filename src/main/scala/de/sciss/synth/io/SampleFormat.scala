/*
 *  SampleFormat.scala
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

import collection.immutable.{IndexedSeq => Vec}

abstract class SampleFormat(val id: String, val bitsPerSample: Int) {
  private[io] def readerFactory: Option[BufferReaderFactory]
  private[io] def writerFactory: Option[BufferWriterFactory]
  private[io] def bidiFactory  : Option[BufferBidiFactory  ]
}

object SampleFormat {
  val fromInt16  = Vec( Int16, Int24, Int32, Float, Double )
  val allSigned  = Int8 +: fromInt16
  val all        = UInt8 +: allSigned

  case object UInt8 extends SampleFormat("uint8", 8) {
    private[io] def readerFactory = Some(BufferReader.Byte)
    private[io] def writerFactory = Some(BufferWriter.Byte)
    private[io] def bidiFactory   = Some(BufferBidi  .Byte)
  }

  case object Int8 extends SampleFormat("int8", 8) {
    private[io] def readerFactory = Some(BufferReader.Byte)
    private[io] def writerFactory = Some(BufferWriter.Byte)
    private[io] def bidiFactory   = Some(BufferBidi  .Byte)
  }

  case object Int16 extends SampleFormat("int16", 16) {
    private[io] def readerFactory = Some(BufferReader.Short)

    private[io] def writerFactory = Some(BufferWriter.Short)

    private[io] def bidiFactory = Some(BufferBidi.Short)
  }

  case object Int24 extends SampleFormat("int24", 24) {
    private[io] def readerFactory = Some(BufferReader.ThreeBytes)

    private[io] def writerFactory = Some(BufferWriter.ThreeBytes)

    private[io] def bidiFactory = Some(BufferBidi.ThreeBytes)
  }

  case object Int32 extends SampleFormat("int32", 32) {
    private[io] def readerFactory = Some(BufferReader.Int)
    private[io] def writerFactory = Some(BufferWriter.Int)
    private[io] def bidiFactory   = Some(BufferBidi  .Int)
  }

  case object Float extends SampleFormat("float", 32) {
    private[io] def readerFactory = Some(BufferReader.Float)
    private[io] def writerFactory = Some(BufferWriter.Float)
    private[io] def bidiFactory   = Some(BufferBidi  .Float)
  }

  case object Double extends SampleFormat("double", 64) {
    private[io] def readerFactory = Some(BufferReader.Double)
    private[io] def writerFactory = Some(BufferWriter.Double)
    private[io] def bidiFactory   = Some(BufferBidi  .Double)
  }

  // case object MuLaw  extends SampleFormat( "mulaw" )
  // case object ALaw   extends SampleFormat( "alaw" )
}
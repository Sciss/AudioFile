/*
 *  SampleFormat.scala
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

package de.sciss.audiofile

import scala.collection.immutable.{IndexedSeq => Vec}

abstract class SampleFormat(val id: String, val bitsPerSample: Int) {
  private[audiofile] def readerFactory: Option[BufferReaderFactory]
  private[audiofile] def writerFactory: Option[BufferWriterFactory]
  private[audiofile] def bidiFactory  : Option[BufferBidiFactory  ]

  private[audiofile] def asyncReaderFactory: Option[AsyncBufferReaderFactory]
  private[audiofile] def asyncWriterFactory: Option[AsyncBufferWriterFactory]
  private[audiofile] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ]
}

object SampleFormat {
  val fromInt16 : Vec[SampleFormat] = Vec(Int16, Int24, Int32, Float, Double)
  val allSigned : Vec[SampleFormat] = Int8  +: fromInt16
  val all       : Vec[SampleFormat] = UInt8 +: allSigned

  case object UInt8 extends SampleFormat("uint8", 8) {
    private[audiofile] def readerFactory = Some(BufferReader.UByte)
    private[audiofile] def writerFactory = Some(BufferWriter.UByte)
    private[audiofile] def bidiFactory   = Some(BufferBidi  .UByte)

    private[audiofile] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .UByte)
    private[audiofile] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .UByte)
    private[audiofile] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .UByte)
  }

  case object Int8 extends SampleFormat("int8", 8) {
    private[audiofile] def readerFactory = Some(BufferReader.Byte)
    private[audiofile] def writerFactory = Some(BufferWriter.Byte)
    private[audiofile] def bidiFactory   = Some(BufferBidi  .Byte)

    private[audiofile] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .Byte)
    private[audiofile] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .Byte)
    private[audiofile] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .Byte)
  }

  case object Int16 extends SampleFormat("int16", 16) {
    private[audiofile] def readerFactory = Some(BufferReader.Short)
    private[audiofile] def writerFactory = Some(BufferWriter.Short)
    private[audiofile] def bidiFactory   = Some(BufferBidi.Short)

    private[audiofile] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .Short)
    private[audiofile] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .Short)
    private[audiofile] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .Short)
  }

  case object Int24 extends SampleFormat("int24", 24) {
    private[audiofile] def readerFactory = Some(BufferReader.ThreeBytes)
    private[audiofile] def writerFactory = Some(BufferWriter.ThreeBytes)
    private[audiofile] def bidiFactory   = Some(BufferBidi  .ThreeBytes)

    private[audiofile] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .ThreeBytes)
    private[audiofile] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .ThreeBytes)
    private[audiofile] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .ThreeBytes)
  }

  case object Int32 extends SampleFormat("int32", 32) {
    private[audiofile] def readerFactory = Some(BufferReader.Int)
    private[audiofile] def writerFactory = Some(BufferWriter.Int)
    private[audiofile] def bidiFactory   = Some(BufferBidi  .Int)

    private[audiofile] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .Int)
    private[audiofile] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .Int)
    private[audiofile] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .Int)
  }

  case object Float extends SampleFormat("float", 32) {
    private[audiofile] def readerFactory = Some(BufferReader.Float)
    private[audiofile] def writerFactory = Some(BufferWriter.Float)
    private[audiofile] def bidiFactory   = Some(BufferBidi  .Float)

    private[audiofile] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .Float)
    private[audiofile] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .Float)
    private[audiofile] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .Float)
  }

  case object Double extends SampleFormat("double", 64) {
    private[audiofile] def readerFactory = Some(BufferReader.Double)
    private[audiofile] def writerFactory = Some(BufferWriter.Double)
    private[audiofile] def bidiFactory   = Some(BufferBidi  .Double)

    private[audiofile] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .Double)
    private[audiofile] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .Double)
    private[audiofile] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .Double)
  }

  // case object MuLaw  extends SampleFormat( "mulaw" )
  // case object ALaw   extends SampleFormat( "alaw" )
}
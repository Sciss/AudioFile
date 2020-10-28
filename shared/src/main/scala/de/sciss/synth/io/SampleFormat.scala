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

package de.sciss.synth.io

import scala.collection.immutable.{IndexedSeq => Vec}

abstract class SampleFormat(val id: String, val bitsPerSample: Int) {
  private[io] def readerFactory: Option[BufferReaderFactory]
  private[io] def writerFactory: Option[BufferWriterFactory]
  private[io] def bidiFactory  : Option[BufferBidiFactory  ]

  private[io] def asyncReaderFactory: Option[AsyncBufferReaderFactory]
  private[io] def asyncWriterFactory: Option[AsyncBufferWriterFactory]
  private[io] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ]
}

object SampleFormat {
  val fromInt16 : Vec[SampleFormat] = Vec(Int16, Int24, Int32, Float, Double)
  val allSigned : Vec[SampleFormat] = Int8  +: fromInt16
  val all       : Vec[SampleFormat] = UInt8 +: allSigned

  case object UInt8 extends SampleFormat("uint8", 8) {
    private[io] def readerFactory = Some(BufferReader.Byte)
    private[io] def writerFactory = Some(BufferWriter.Byte)
    private[io] def bidiFactory   = Some(BufferBidi  .Byte)

    private[io] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .Byte)
    private[io] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .Byte)
    private[io] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .Byte)
  }

  case object Int8 extends SampleFormat("int8", 8) {
    private[io] def readerFactory = Some(BufferReader.UByte)
    private[io] def writerFactory = Some(BufferWriter.UByte)
    private[io] def bidiFactory   = Some(BufferBidi  .UByte)

    private[io] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .UByte)
    private[io] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .UByte)
    private[io] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .UByte)
  }

  case object Int16 extends SampleFormat("int16", 16) {
    private[io] def readerFactory = Some(BufferReader.Short)
    private[io] def writerFactory = Some(BufferWriter.Short)
    private[io] def bidiFactory   = Some(BufferBidi.Short)

    private[io] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .Short)
    private[io] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .Short)
    private[io] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .Short)
  }

  case object Int24 extends SampleFormat("int24", 24) {
    private[io] def readerFactory = Some(BufferReader.ThreeBytes)
    private[io] def writerFactory = Some(BufferWriter.ThreeBytes)
    private[io] def bidiFactory   = Some(BufferBidi.ThreeBytes)

    private[io] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .ThreeBytes)
    private[io] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .ThreeBytes)
    private[io] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .ThreeBytes)
  }

  case object Int32 extends SampleFormat("int32", 32) {
    private[io] def readerFactory = Some(BufferReader.Int)
    private[io] def writerFactory = Some(BufferWriter.Int)
    private[io] def bidiFactory   = Some(BufferBidi  .Int)

    private[io] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .Int)
    private[io] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .Int)
    private[io] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .Int)
  }

  case object Float extends SampleFormat("float", 32) {
    private[io] def readerFactory = Some(BufferReader.Float)
    private[io] def writerFactory = Some(BufferWriter.Float)
    private[io] def bidiFactory   = Some(BufferBidi  .Float)

    private[io] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .Float)
    private[io] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .Float)
    private[io] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .Float)
  }

  case object Double extends SampleFormat("double", 64) {
    private[io] def readerFactory = Some(BufferReader.Double)
    private[io] def writerFactory = Some(BufferWriter.Double)
    private[io] def bidiFactory   = Some(BufferBidi  .Double)

    private[io] def asyncReaderFactory: Option[AsyncBufferReaderFactory] = Some(AsyncBufferReader .Double)
    private[io] def asyncWriterFactory: Option[AsyncBufferWriterFactory] = Some(AsyncBufferWriter .Double)
    private[io] def asyncBidiFactory  : Option[AsyncBufferBidiFactory  ] = Some(AsyncBufferBidi   .Double)
  }

  // case object MuLaw  extends SampleFormat( "mulaw" )
  // case object ALaw   extends SampleFormat( "alaw" )
}
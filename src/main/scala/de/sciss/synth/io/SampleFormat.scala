/*
 *  SampleFormat.scala
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	 For further information, please contact Hanns Holger Rutz at
 *	 contact@sciss.de
 */

package de.sciss.synth.io

abstract class SampleFormat( val id: String, val bitsPerSample: Int ) {
   private[io] def readerFactory  : Option[ BufferReaderFactory ]
   private[io] def writerFactory  : Option[ BufferWriterFactory ]
   private[io] def bidiFactory    : Option[ BufferBidiFactory ]
}

object SampleFormat {
   case object UInt8 extends SampleFormat( "uint8", 8 ) {
      private[io] def readerFactory = Some( BufferReader.Byte )
      private[io] def writerFactory = Some( BufferWriter.Byte )
      private[io] def bidiFactory   = Some( BufferBidi.Byte )
   }
   case object Int8 extends SampleFormat( "int8", 8 ) {
      private[io] def readerFactory = Some( BufferReader.Byte )
      private[io] def writerFactory = Some( BufferWriter.Byte )
      private[io] def bidiFactory   = Some( BufferBidi.Byte )
   }
   case object Int16 extends SampleFormat( "int16", 16 ) {
      private[io] def readerFactory = Some( BufferReader.Short )
      private[io] def writerFactory = Some( BufferWriter.Short )
      private[io] def bidiFactory   = Some( BufferBidi.Short )
   }
   case object Int24 extends SampleFormat( "int24", 24 ) {
      private[io] def readerFactory = Some( BufferReader.ThreeBytes )
      private[io] def writerFactory = Some( BufferWriter.ThreeBytes )
      private[io] def bidiFactory   = Some( BufferBidi.ThreeBytes )
   }
   case object Int32 extends SampleFormat( "int32", 32 ) {
      private[io] def readerFactory = Some( BufferReader.Int )
      private[io] def writerFactory = Some( BufferWriter.Int )
      private[io] def bidiFactory   = Some( BufferBidi.Int )
   }
   case object Float extends SampleFormat( "float", 32 ) {
      private[io] def readerFactory = Some( BufferReader.Float )
      private[io] def writerFactory = Some( BufferWriter.Float )
      private[io] def bidiFactory   = Some( BufferBidi.Float )
   }
   case object Double extends SampleFormat( "double", 64 ) {
      private[io] def readerFactory = Some( BufferReader.Double )
      private[io] def writerFactory = Some( BufferWriter.Double )
      private[io] def bidiFactory   = Some( BufferBidi.Double )
   }
// case object MuLaw  extends SampleFormat( "mulaw" )
// case object ALaw   extends SampleFormat( "alaw" )
}
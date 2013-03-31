/*
 *  AudioFileHeader.java
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2013 Hanns Holger Rutz. All rights reserved.
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

import java.nio.ByteOrder
import java.io.{ DataInput, DataInputStream, DataOutput, DataOutputStream, IOException, RandomAccessFile }

private[io] object AudioFileHeader {
   def opNotSupported = throw new IOException( "Operation not supported" )

   @throws( classOf[ IOException ])
   @inline def readLittleUShort( din: DataInput ) : Int = {
      val i = din.readUnsignedShort()
      (i >> 8) | ((i & 0xFF) << 8)
   }

   @throws( classOf[ IOException ])
   @inline def readLittleInt( din: DataInput ) : Int = {
      val i = din.readInt()
      ((i>> 24) & 0xFF) | ((i >> 8) & 0xFF00) | ((i << 8) & 0xFF0000) | (i << 24)
   }

   @throws( classOf[ IOException ])
   @inline def readLittleFloat( din: DataInput ) : Float = {
      val i = din.readInt()
      java.lang.Float.intBitsToFloat( ((i>> 24) & 0xFF) | ((i >> 8) & 0xFF00) | ((i << 8) & 0xFF0000) | (i << 24) )
   }

   @throws( classOf[ IOException ])
   @inline def readLittleLong( din: DataInput ) : Long = {
      val n = din.readLong()
      ((n >> 56) & 0xFFL) |
            ((n >> 40) & 0xFF00L) |
            ((n >> 24) & 0xFF0000L) |
            ((n >>  8) & 0xFF000000L) |
            ((n <<  8) & 0xFF00000000L) |
            ((n << 24) & 0xFF0000000000L) |
            ((n << 40) & 0xFF000000000000L) |
             (n << 56)
   }

   @throws( classOf[ IOException ])
   @inline def writeLittleShort( dout: DataOutput, i: Int ) {
      dout.writeShort( (i >> 8) | ((i& 0xFF) << 8) )
   }

   @throws( classOf[ IOException ])
   @inline def writeLittleInt( dout: DataOutput, i: Int ) {
      dout.writeInt( ((i >> 24) & 0xFF) | ((i >> 8) & 0xFF00) | ((i << 8)& 0xFF0000) | (i << 24) )
   }

   @throws( classOf[ IOException ])
   @inline def writeLittleFloat( dout: DataOutput, f: Float ) {
      val i = java.lang.Float.floatToIntBits( f )
      dout.writeInt( ((i >> 24) & 0xFF) | ((i >> 8) & 0xFF00) | ((i << 8)& 0xFF0000) | (i << 24) )
   }

   @throws( classOf[ IOException ])
   @inline def writeLittleLong( dout: DataOutput, n: Long ) {
      dout.writeLong( ((n >> 56) & 0xFFL) |
                     ((n >> 40) & 0xFF00L) |
                     ((n >> 24) & 0xFF0000L) |
                     ((n >>  8) & 0xFF000000L) |
                     ((n <<  8) & 0xFF00000000L) |
                     ((n << 24) & 0xFF0000000000L) |
                     ((n << 40) & 0xFF000000000000L) |
                      (n << 56) )
   }

   @throws( classOf[ IOException ])
   def readNullTermString( din: DataInput ) : String = {
      val buf  = new StringBuffer()
      var b    = din.readByte()
      while( b != 0 ) {
         buf.append( b.toChar )
         b	= din.readByte()
      }
      buf.toString
   }

   def formatError()       = throw new IOException( "A header format error occurred" )
   def encodingError()     = throw new IOException( "File has unsupported encoding" )
   def incompleteError()   = throw new IOException( "Header data is incomplete" )

   trait DataInputReader {
      @throws( classOf[ IOException ]) def readInt() : Int
      @throws( classOf[ IOException ]) def readFloat() : Float
      def byteOrder : ByteOrder
   }

   trait DataOutputWriter {
      @throws( classOf[ IOException ]) def writeInt( i: Int ) : Unit
      @throws( classOf[ IOException ]) def writeFloat( f: Float ) : Unit
      def byteOrder : ByteOrder
   }

   def dataInputReader( din: DataInput, byteOrder: ByteOrder ) : DataInputReader = {
      if( byteOrder == ByteOrder.LITTLE_ENDIAN ) {
         new LittleDataInputReader( din )
      } else {
         new BigDataInputReader( din )
      }
   }

   def nativeDataInputReader( din: DataInput ) : DataInputReader = dataInputReader( din, ByteOrder.nativeOrder )

   def dataOutputWriter( dout: DataOutput, byteOrder: ByteOrder ) : DataOutputWriter = {
      if( byteOrder == ByteOrder.LITTLE_ENDIAN ) {
         new LittleDataOutputWriter( dout )
      } else {
         new BigDataOutputWriter( dout )
      }
   }

   def nativeDataOutputWriter( dout: DataOutput ) : DataOutputWriter = dataOutputWriter( dout, ByteOrder.nativeOrder )

   final class BigDataInputReader( din: DataInput ) extends DataInputReader {
      @throws( classOf[ IOException ]) def readInt()    = din.readInt()
      @throws( classOf[ IOException ]) def readFloat()  = din.readFloat()
      def byteOrder  = ByteOrder.BIG_ENDIAN
   }

   final class LittleDataInputReader( din: DataInput ) extends DataInputReader {
      @throws( classOf[ IOException ]) def readInt()    = readLittleInt( din )
      @throws( classOf[ IOException ]) def readFloat()  = readLittleFloat( din )
      def byteOrder  = ByteOrder.LITTLE_ENDIAN
   }

   final class BigDataOutputWriter( dout: DataOutput ) extends DataOutputWriter {
      @throws( classOf[ IOException ]) def writeInt( i: Int )     { dout.writeInt( i )}
      @throws( classOf[ IOException ]) def writeFloat( f: Float ) { dout.writeFloat( f )}
      def byteOrder  = ByteOrder.BIG_ENDIAN
   }

   final class LittleDataOutputWriter( dout: DataOutput ) extends DataOutputWriter {
      @throws( classOf[ IOException ]) def writeInt( i: Int )     { writeLittleInt( dout, i )}
      @throws( classOf[ IOException ]) def writeFloat( f: Float ) { writeLittleFloat( dout, f )}
      def byteOrder  = ByteOrder.LITTLE_ENDIAN
   }
}

//private[io] trait AudioFileHeaderFactory {
//   def createHeaderReader : Option[ AudioFileHeaderReader ]
//   def createHeaderWriter : Option[ AudioFileHeaderWriter ]
//
//   @throws( classOf[ IOException ])
//   def identify( dis: DataInputStream ) : Boolean
//}

private[io] trait AudioFileHeader {
   def byteOrder : ByteOrder
   def spec : AudioFileSpec
}

private[io] case class ReadableAudioFileHeader( spec: AudioFileSpec, byteOrder: ByteOrder )
extends AudioFileHeader

private[io] trait WritableAudioFileHeader extends AudioFileHeader {
   @throws( classOf[ IOException ])
   def update( numFrames: Long ) : Unit
}

//trait ReadableAudioFileHeader extends AudioFileHeader {
//   def createBufferReader( bufSize: Int ) : Option[ BufferReader ]
//}

private[io] trait AudioFileHeaderReader {
   /**
    *    Reads in the header information. Seek position
    *    should remain at the first frame of the audio data.
    *    If the header does not support a plain input stream
    *    but requires random access or length information,
    *    it should throw an IOException
    */
   @throws( classOf[ IOException ])
   def read( dis: DataInputStream ) : AudioFileHeader

   @throws( classOf[ IOException ])
   def read( raf: RandomAccessFile ) : AudioFileHeader

//   // WAV and AIFF might overwrite this
//   @throws( classOf[ IOException ])
//   def readMarkers { /* empty */ }
//
//   // AIFF might overwrite this
//   @throws( classOf[ IOException ])
//   def readAppCode { /* empty */ }
}

private[io] trait AudioFileHeaderWriter {
   @throws( classOf[ IOException ])
   def write( dos: DataOutputStream, spec: AudioFileSpec ) : WritableAudioFileHeader

   @throws( classOf[ IOException ])
   def write( raf: RandomAccessFile, spec: AudioFileSpec ) : WritableAudioFileHeader

//   @throws( classOf[ IOException ])
//   def update( numFrames: Long ) : Unit
//
//   def createBufferWriter : Option[ BufferWriter ]
//   def createBufferHandler : Option[ BufferHandler ]
}
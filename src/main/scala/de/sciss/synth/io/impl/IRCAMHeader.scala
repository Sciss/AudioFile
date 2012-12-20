/*
 *  AIFFHeader.scala
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
package impl

import java.nio.ByteOrder
import java.io._
import annotation.switch

/**
 * http://www-mmsp.ece.mcgill.ca/documents/audioformats/IRCAM/IRCAM.html
 */
private[io] object IRCAMHeader /* extends BasicHeader */ {

   private final val IRCAM_VAXLE_MAGIC    = 0x64A30100
   private final val IRCAM_VAXBE_MAGIC    = 0x0001A364
   private final val IRCAM_SUNBE_MAGIC    = 0x64A30200
   private final val IRCAM_SUNLE_MAGIC    = 0x0002A364
   private final val IRCAM_MIPSLE_MAGIC   = 0x64A30300
   private final val IRCAM_MIPSBE_MAGIC   = 0x0003A364
   private final val IRCAM_NEXTBE_MAGIC   = 0x64A30400

   private final val BICSF_END            = 0
// private final val BICSF_MAXAMP         = 1
// private final val BICSF_COMMENT        = 2
   private final val BICSF_LINKCODE		   = 3
   private final val BICSF_VIRTUALCODE    = 4
// private final val BICSF_CUECODE        = 8
// private final val BICSF_PARENTCODE     = 11

   @throws( classOf[ IOException ])
   def identify( dis: DataInputStream ) = {
      val magic = dis.readInt()
      (magic == IRCAM_VAXLE_MAGIC  || magic == IRCAM_VAXBE_MAGIC ||
       magic == IRCAM_SUNBE_MAGIC  || magic == IRCAM_SUNLE_MAGIC ||
       magic == IRCAM_MIPSLE_MAGIC || magic == IRCAM_MIPSBE_MAGIC ||
       magic == IRCAM_NEXTBE_MAGIC)
   }

   import AudioFileHeader._

   @throws( classOf[ IOException ])
   def read( raf: RandomAccessFile ) : AudioFileHeader = readDataInput( raf, raf.length() )

   @throws( classOf[ IOException ])
   def read( dis: DataInputStream ) : AudioFileHeader = readDataInput( dis, dis.available() )

   @throws( classOf[ IOException ])
   private def readDataInput( din: DataInput, fileLen: Long ) : AudioFileHeader = {
      val magic = din.readInt()
      val reader = if( magic == IRCAM_VAXLE_MAGIC || magic == IRCAM_SUNLE_MAGIC || magic == IRCAM_MIPSLE_MAGIC ) {
         new LittleDataInputReader( din )
      } else if( magic == IRCAM_VAXBE_MAGIC || magic == IRCAM_SUNBE_MAGIC || magic == IRCAM_MIPSBE_MAGIC || magic == IRCAM_NEXTBE_MAGIC ) {
         new BigDataInputReader( din )
      } else formatError()
      val sampleRate    = reader.readFloat()
      val numChannels   = reader.readInt()
      val sampleFormat  = (reader.readInt(): @switch) match {
         case 1         => SampleFormat.Int8    // 8 bitlinear
         case 2         => SampleFormat.Int16   // 16 bit linear
         case 3         => SampleFormat.Int24   // 24 bit linear; existiert dieser wert offiziell?
         case 0x40004   => SampleFormat.Int32	// 32 bit linear
         case 4         => SampleFormat.Float	// 32 bit float
         case 8         => SampleFormat.Double  // 64 bit float
         case m         => throw new IOException( "Unsupported IRCAM encoding (" + m + ")" )
      }

      var done = false
      var pos  = 16L

      while( !done ) {
         val i    = reader.readInt()
         val sz   = i & 0xFFFF   // last short = block size
         val id   = i >> 16      // first short = code
         if( id == BICSF_END ) {
            done = true
         } else if( id == BICSF_LINKCODE ) {
            throw new IOException( "Unsupported IRCAM feature (LINKCODE)" )
         } else if( id == BICSF_VIRTUALCODE ) {
            throw new IOException( "Unsupported IRCAM feature (VIRTUALCODE)" )
         }

         if( sz > 0 ) din.skipBytes( sz )
         pos += 4 + sz
      }

      val dataOffset = (pos + 1023L) & ~1023L   // skip to next full kilobyte
      val skp        = (dataOffset - pos).toInt   // skip to next full kilobyte
      if( skp > 0 ) din.skipBytes( skp )
      val frameSize  = ((sampleFormat.bitsPerSample + 7) >> 3) * numChannels
      val numFrames  = math.max( 0L, fileLen - dataOffset ) / frameSize

      val spec = new AudioFileSpec( AudioFileType.IRCAM, sampleFormat, numChannels, sampleRate, Some( reader.byteOrder ), numFrames )
      ReadableAudioFileHeader( spec, reader.byteOrder )
   }

   @throws( classOf[ IOException ])
   def write( raf: RandomAccessFile, spec: AudioFileSpec ) : WritableAudioFileHeader = {
      val spec1 = writeDataOutput( raf, spec )
      new NonUpdatingWritableHeader( spec1 )
   }

   @throws( classOf[ IOException ])
   def write( dos: DataOutputStream, spec: AudioFileSpec ) : WritableAudioFileHeader = {
      val spec1 = writeDataOutput( dos, spec )
      new NonUpdatingWritableHeader( spec1 )
   }

   @throws( classOf[ IOException ])
   private def writeDataOutput( dout: DataOutput, spec: AudioFileSpec ) : AudioFileSpec = {
      val writer     = dataOutputWriter( dout, spec.byteOrder.getOrElse( ByteOrder.nativeOrder ))
      val byteOrder  = spec.byteOrder.getOrElse( ByteOrder.nativeOrder )
      dout.writeInt( if( byteOrder == ByteOrder.LITTLE_ENDIAN ) IRCAM_VAXLE_MAGIC else IRCAM_SUNBE_MAGIC )
      writer.writeFloat( spec.sampleRate.toFloat )
      writer.writeInt( spec.numChannels )
      writer.writeInt( if( spec.sampleFormat == SampleFormat.Int32 ) {
         0x40004
      } else {
         // 1 = 8bit int, 2 = 16bit lin; 3 = 24 bit, 4 = 32bit float, 8 = 64bit float
         spec.sampleFormat.bitsPerSample >> 3
      })
      var pos = 16L

      writer.writeInt( BICSF_END << 16 )
      pos += 4
      val dataOffset = (pos + 1023L) & ~1023L   // rounded up to full kilobyte
      val skp = (dataOffset - pos).toInt
      if( skp > 0 ) dout.write( new Array[ Byte ]( skp ))  // pad until sample off

      spec.copy( byteOrder = Some( byteOrder ))
   }
}
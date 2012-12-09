/*
 *  NeXTHeader.scala
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

import java.io.{DataInput, DataInputStream, RandomAccessFile, IOException}
import annotation.switch
import java.nio.ByteOrder

/**
 * http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/AU/AU.html
 */
private[io] object NeXTHeader extends AudioFileHeaderFactory {
   private final val SND_MAGIC		= 0x2E736E64   // '.snd'

   // ---- AudioFileHeaderFactory ----
   def createHeaderReader : Option[ AudioFileHeaderReader ] = Some( new Reader )
   def createHeaderWriter : Option[ AudioFileHeaderWriter ] = None   // XXX Some( new Writer )

   @throws( classOf[ IOException ])
   def identify( dis: DataInputStream ) = dis.readInt() == SND_MAGIC

   private class Reader extends AudioFileHeaderReader {
      import AudioFileHeader._

      @throws( classOf[ IOException ])
      def read( raf: RandomAccessFile ) : AudioFileHeader = readDataInput( raf, raf.length() )

      @throws( classOf[ IOException ])
      def read( dis: DataInputStream ) : AudioFileHeader = readDataInput( dis, dis.available() )

      @throws( classOf[ IOException ])
      private def readDataInput( din: DataInput, fileLen: Long ) : AudioFileHeader = {
         if( din.readInt() != SND_MAGIC ) formatError()

         val dataOffset       = din.readInt()   // offset in bytes
         val dataSize_?       = din.readInt()
         val sampleFormat     = (din.readInt(): @switch) match {
            case 2         => SampleFormat.Int8    // 8 bit linear
            case 3         => SampleFormat.Int16   // 16 bit linear
            case 4         => SampleFormat.Int24   // 24 bit linear
            case 5         => SampleFormat.Int32	// 32 bit linear
            case 6         => SampleFormat.Float	// 32 bit float
            case 7         => SampleFormat.Double  // 64 bit float
            case m         => throw new IOException( "Unsupported NeXT encoding (" + m + ")" )
         }
         val sampleRate       = din.readInt().toDouble
         val numChannels      = din.readInt()

         val skp              = dataOffset - 24 // current pos is 24
         if( skp > 0 ) din.skipBytes( skp )
         val frameSize        = ((sampleFormat.bitsPerSample + 7) >> 3) * numChannels

         val dataSize         = if( dataSize_? == 0xFFFFFFFF ) fileLen - dataOffset else dataSize_?
         val numFrames        = math.max( 0L, dataSize ) / frameSize

         val spec = new AudioFileSpec( AudioFileType.NeXT, sampleFormat, numChannels, sampleRate, Some( ByteOrder.BIG_ENDIAN ), numFrames )
         ReadableAudioFileHeader( spec, ByteOrder.BIG_ENDIAN )
      }
   }
}

/*
	private class SNDHeader
	extends AudioFileHeader
	{
		private static final int SND_MAGIC		= 0x2E736E64;	// '.snd'

		private long sampleDataOffset;
		private long headDataLenOffset= 8L;
		private long lastUpdateLength = 0L;

		protected SNDHeader() {  }

		protected void readHeader( AudioFileInfo descr )
		throws IOException
		{
			int		i1,i2;
			String	str;

			raf.readInt();  // SND magic
			sampleDataOffset= raf.readInt();
			i2				= raf.readInt();
		i1				= raf.readInt();
			descr.rate		= raf.readInt();
			descr.channels	=raf.readInt();
			str				= readNullTermString();

			if( str.len() > 0 ) descr.setProperty( AudioFileInfo.KEY_COMMENT, str );

			switch( i1 ) {
	case 2:	// 8 bit linear
				descr.bitsPerSample	= 8;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 3:	// 16 bit linear
				descr.bitsPerSample	= 16;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 4:	// 24 bit linear
				descr.bitsPerSample	= 24;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 5:	// 32 bit linear
				descr.bitsPerSample	= 32;
				descr.sampleFormat	=AudioFileInfo.FORMAT_INT;
				break;
		case 6:	// 32 bit float
				descr.bitsPerSample	= 32;
				descr.sampleFormat	= AudioFileInfo.FORMAT_FLOAT;
				break;
			case 7:	// 64 bit float
				descr.bitsPerSample	= 64;
				descr.sampleFormat	= AudioFileInfo.FORMAT_FLOAT;
				break;
			default:
				throw new IOException( getResourceString( "errAudioFileEncoding" ));
			}

			descr.len	= i2 / (((descr.bitsPerSample + 7) >> 3) * descr.channels);
		}

		protected void writeHeader( AudioFileInfo descr )
		throws IOException
		{
			String str;

			str				= (String) descr.getProperty( AudioFileInfo.KEY_COMMENT );
			sampleDataOffset	= str == null ? 28L : (long) ((28 +str.len()) & ~3);
			raf.writeInt( SND_MAGIC );
			raf.writeInt( (int) sampleDataOffset );
//			raf.writeInt( stream.samples * frameLength );	// len
			raf.writeInt( 0 );

			if( descr.sampleFormat == AudioFileInfo.FORMAT_INT ){
				raf.writeInt( (descr.bitsPerSample >> 3) + 1 );
			} else {
raf.writeInt( (descr.bitsPerSample >> 5) + 5 );
			}
raf.writeInt((int) (descr.rate + 0.5) );
			raf.writeInt( descr.channels );

			// comment
			if( str == null ) {
				raf.writeInt( 0 );  // minimum 4 byte character data
			} else {
				raf.writeBytes( str );
				switch( str.len() & 3 ) {
			case 0:
				raf.writeInt( 0 );
					break;
				case 1:
					raf.writeByte( 0 );
					raf.writeShort( 0 );
					break;
				case 2:
					raf.writeShort( 0 );
					break;
				case 3:
			raf.writeByte( 0 );
					break;
				}
			}

//			updateHeader( afd );
		}

	protected voidupdateHeader( AudioFileInfo descr )
		throws IOException
		{
			long oldPos;
			long len	= raf.len();
			if( len == lastUpdateLength ) return;

		if( len >= headDataLenOffset+ 4 ) {
				oldPos = raf.getFilePointer();
			raf.seek( headDataLenOffset );
				raf.writeInt( (int) (len - sampleDataOffset) );		// data size
				raf.seek( oldPos );
				lastUpdateLength = len;
			}
		}

		protected long getSampleDataOffset()
		{
			return sampleDataOffset;
		}

		protected ByteOrder getByteOrder()
		{
			return ByteOrder.BIG_ENDIAN;
		}
	} // class SNDHeader
*/
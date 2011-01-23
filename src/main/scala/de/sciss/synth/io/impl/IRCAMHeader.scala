/*
 *  AIFFHeader.scala
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2011 Hanns Holger Rutz. All rights reserved.
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
 *
 *
 *  Changelog:
 */

package de.sciss.synth.io
package impl

import java.nio.ByteOrder
import java.io._

private[io] object IRCAMHeader extends AudioFileHeaderFactory {

   private val IRCAM_VAXLE_MAGIC	   = 0x64A30100
   private val IRCAM_VAXBE_MAGIC	   = 0x0001A364
   private val IRCAM_SUNBE_MAGIC	   = 0x64A30200
   private val IRCAM_SUNLE_MAGIC	   = 0x0002A364
   private val IRCAM_MIPSLE_MAGIC   = 0x64A30300
   private val IRCAM_MIPSBE_MAGIC   = 0x0003A364
   private val IRCAM_NEXTBE_MAGIC   = 0x64A30400

   private val BICSF_END            = 0
//		private val BICSF_MAXAMP			= 1
//   private val BICSF_COMMENT		= 2
   private val BICSF_LINKCODE		   = 3
   private val BICSF_VIRTUALCODE    = 4
//   private val BICSF_CUECODE		= 8
//		private val BICSF_PARENTCODE		= 11

   // ---- AudioFileHeaderFactory ----
   def createHeaderReader : Option[ AudioFileHeaderReader ] = Some( new Reader )
   def createHeaderWriter : Option[ AudioFileHeaderWriter ] = Some( new Writer )

   @throws( classOf[ IOException ])
   def identify( dis: DataInputStream ) = {
      val magic = dis.readInt()
      (magic == IRCAM_VAXLE_MAGIC  || magic == IRCAM_VAXBE_MAGIC ||
       magic == IRCAM_SUNBE_MAGIC  || magic == IRCAM_SUNLE_MAGIC ||
       magic == IRCAM_MIPSLE_MAGIC || magic == IRCAM_MIPSBE_MAGIC ||
       magic == IRCAM_NEXTBE_MAGIC)
   }

   private class Reader extends AudioFileHeaderReader {
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
         } else formatError
         val sampleRate    = reader.readFloat()
         val numChannels   = reader.readInt()
         val sampleFormat  = reader.readInt() match {
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
//            i1 match {
//            case BICSF_CUECODE =>
//               if( strBuf == null ) {
//                  strBuf = new Array[ Byte ]( 64 )			// tostore the names
//               }
//               raf.readFully( strBuf );				// region name
//               for( i3 = 0; i3 < 64; i3++ ) {
//                  if( strBuf[ i3 ] ==0 ) break;
//               }
//               i1	= raf.readInt();					// begin smp
//               i2	= raf.readInt();					// endsmp
//               regions.add( new Region( new Span( i1, i2 ), new String( strBuf, 0, i3 )));
//            case BICSF_COMMENT =>
//               strBuf2	= new Array[ Byte ]( i2 )
//               raf.readFully( strBuf2 );
//               descr.setProperty( AudioFileInfo.KEY_COMMENT, new String( strBuf2 ));
//            case _ =>

            if( sz > 0 ) din.skipBytes( sz )
            pos += 4 + sz
         }

//         if( !regions.isEmpty() ) {
//            descr.setProperty( AudioFileInfo.KEY_REGIONS, regions );
//         }

         val dataOffset = (pos + 1023L) & ~1023L   // skip to next full kilobyte
         val skp        = (dataOffset - pos).toInt   // skip to next full kilobyte
         if( skp > 0 ) din.skipBytes( skp )
         val frameSize  = ((sampleFormat.bitsPerSample + 7) >> 3) * numChannels
         val numFrames  = math.max( 0L, fileLen - dataOffset ) / frameSize

         val spec = new AudioFileSpec( AudioFileType.IRCAM, sampleFormat, numChannels, sampleRate, Some( reader.byteOrder ), numFrames )
         ReadableAudioFileHeader( spec, reader.byteOrder )
      }
   }

   private class Writer extends AudioFileHeaderWriter {
      import AudioFileHeader._

      @throws( classOf[ IOException ])
      def write( raf: RandomAccessFile, spec: AudioFileSpec ) : WritableAudioFileHeader = {
         val spec1 = writeDataOutput( raf, spec )
         new WritableHeader( spec1 )
      }

      @throws( classOf[ IOException ])
      def write( dos: DataOutputStream, spec: AudioFileSpec ) : WritableAudioFileHeader = {
         val spec1 = writeDataOutput( dos, spec )
         new WritableHeader( spec1 )
      }

      @throws( classOf[ IOException ])
      private def writeDataOutput( dout: DataOutput, spec: AudioFileSpec ) : AudioFileSpec = {
         val writer     = dataOutputWriter( dout, spec.byteOrder.getOrElse( ByteOrder.nativeOrder ))
         val byteOrder  = spec.byteOrder.getOrElse( ByteOrder.nativeOrder )
         dout.writeInt( if( byteOrder == ByteOrder.LITTLE_ENDIAN ) IRCAM_VAXLE_MAGIC else IRCAM_SUNBE_MAGIC )
         writer.writeFloat( spec.sampleRate.toFloat )
         writer.writeInt( spec.numChannels )
         writer.writeInt( if( spec.sampleFormat == SampleFormat.Float ) 0x40004 else
            spec.sampleFormat.bitsPerSample >> 3   // 1 = 8bit int, 2 = 16bit lin; 3 = 24 bit, 4 = 32bit float, 8 = 64bit float
         )
         var pos = 16L

//         // markers + regions, loop
//         regions  = (List) descr.getProperty( AudioFileInfo.KEY_REGIONS );
//         if( regions != null &&!regions.isEmpty() ) {
//            i1= (BICSF_CUECODE << 16) + 72;		// short cue-code, short sizeof-cuepoint (64 + 4 + 4)
//            strBuf	= new byte[ 64 ];
//            strBuf[ 0 ] = 0;
//            for( i2 = 0; i2 < regions.size(); i2++ ) {
//   region	= (Region) regions.get( i2 );
//               raf.writeInt( i1 );		// chunk header
//               if( region.name.len() <= 64 ) {
//                  raf.writeBytes( region.name );
//                  raf.write( strBuf, 0, 64 - region.name.len() );
//               } else {
//         raf.writeBytes( region.name.substring( 0, 64 ));
//               }
//            raf.writeInt( (int) region.span.getStart() );
//      raf.writeInt( (int) region.span.getStop());
//            }
//         }

//			// comment
//			str	= (String) descr.getProperty( AudioFileInfo.KEY_COMMENT );
//			if( str != null ) {
//				i1		= (BICSF_COMMENT << 16) | str.len();
//				raf.writeInt(i1 );
//				raf.writeBytes( str );
//			}

			writer.writeInt( BICSF_END << 16 )
         pos += 4
         val dataOffset = (pos + 1023L) & ~1023L   // rounded up to full kilobyte
         val skp = (dataOffset - pos).toInt
         if( skp > 0 ) dout.write( new Array[ Byte ]( skp ))  // pad until sample off

         spec.copy( byteOrder = Some( byteOrder ))
      }
   }

   private class WritableHeader( spec0: AudioFileSpec )
   extends WritableAudioFileHeader {
      private var numFrames0 = spec0.numFrames    // XXX do we need to sync this because its a long?

      @throws( classOf[ IOException ])
      def update( numFrames: Long ) {
//         spec = spec.copy( numFrames = numFrames )
         numFrames0 = numFrames
      }

      def spec = spec0.copy( numFrames = numFrames0 )
      def byteOrder = spec0.byteOrder.get
   }
}

/*
	private class IRCAMHeader
	extends AudioFileHeader
	{
		// http://www.tsp.ece.mcgill.ca/MMSP/Documents/AudioFormats/IRCAM/IRCAM.html
		// for details about the different magic cookies
	private static final int IRCAM_VAXBE_MAGIC		= 0x0001A364;
		private static final int IRCAM_SUNBE_MAGIC		= 0x64A30200;
		private static final int IRCAM_MIPSBE_MAGIC		= 0x0003A364;

		private static final short BICSF_END= 0;
//		private static finalshort BICSF_MAXAMP			= 1;
		private static final short BICSF_COMMENT		= 2;
		private static final short BICSF_LINKCODE		= 3;
		private static final short BICSF_VIRTUALCODE= 4;
		private static final short BICSF_CUECODE		= 8;
//		private static final short BICSF_PARENTCODE		= 11;

		private long sampleDataOffset;

		protected IRCAMHeader() { }
		protected void writeHeader( AudioFileInfo descr )
		throws IOException
		{
			int				i1, i2;
			List			regions;
			Region			region;
			byte[]			strBuf;
			long			pos;
			String			str;

			raf.writeInt( IRCAM_VAXBE_MAGIC );
	raf.writeFloat( (float) descr.rate );
			raf.writeInt( descr.channels );

			if( (descr.sampleFormat == AudioFileInfo.FORMAT_INT) && (descr.bitsPerSample == 32) ) {
				i1 =0x40004;
			} else {
				i1	= descr.bitsPerSample >> 3;		// 1 = 8bit int, 2 = 16bit lin; 3 = 24 bit, 4 = 32bit float, 8 = 64bit float
			}
			raf.writeInt(i1 );

			// markers + regions, loop
			regions  = (List) descr.getProperty( AudioFileInfo.KEY_REGIONS );
			if( regions != null &&!regions.isEmpty() ) {
				i1= (BICSF_CUECODE << 16) + 72;		// short cue-code, short sizeof-cuepoint (64 + 4 + 4)
				strBuf	= new byte[ 64 ];
				strBuf[ 0 ] = 0;
				for( i2 = 0; i2 < regions.size(); i2++ ) {
	region	= (Region) regions.get( i2 );
					raf.writeInt( i1 );		// chunk header
					if( region.name.len() <= 64 ) {
						raf.writeBytes( region.name );
						raf.write( strBuf, 0, 64 - region.name.len() );
					} else {
			raf.writeBytes( region.name.substring( 0, 64 ));
					}
				raf.writeInt( (int) region.span.getStart() );
		raf.writeInt( (int) region.span.getStop());
				}
			}

			// comment
			str	= (String) descr.getProperty( AudioFileInfo.KEY_COMMENT );
			if( str != null ) {
				i1		= (BICSF_COMMENT << 16) | str.len();
				raf.writeInt(i1 );
				raf.writeBytes( str );
			}

			raf.writeInt( BICSF_END << 16 );
			pos				= raf.getFilePointer();
			sampleDataOffset= (pos + 1023L) & ~1023L;		// aufgerundet auf ganze kilobyte
			strBuf			= new byte[ (int) (sampleDataOffset - pos) ];
			raf.write( strBuf );							// pad until sample off
		}

		protected void updateHeader( AudioFileInfo descr )
		throws IOException
		{
			// not necessary
		}

		protected long getSampleDataOffset()
		{
			return sampleDataOffset;
		}

		protected ByteOrder getByteOrder()
		{
		return ByteOrder.BIG_ENDIAN;	// XXX at the moment only big endian is supported
}
	} // class IRCAMHeader
*/
/*
 *  WaveHeader.java
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

import java.io.{DataOutput, DataOutputStream, DataInput, DataInputStream, EOFException, IOException, RandomAccessFile}
import java.nio.ByteOrder
import annotation.switch

private[io] trait AbstractRIFFHeader {
   protected final val ADTL_MAGIC		= 0x6164746C	// 'adtl'
   protected final val LABL_MAGIC		= 0x6C61626C	// 'labl'
   protected final val LTXT_MAGIC		= 0x6C747874	// 'ltxt'

   // ltxt purpose for regions
   protected final val RGN_MAGIC		   = 0x72676E20	// 'rgn '

   // fmt format-code
   protected final val FORMAT_PCM		= 0x0001
   protected final val FORMAT_FLOAT		= 0x0003
   protected final val FORMAT_EXT		= 0xFFFE
//
//   protected long 		sampleDataOffset;
//   protected long		dataLengthOffset;
//   protected long		factSmpNumOffset;
//   protected long		lastUpdateLength	= 0L;
//   protected boolean	isFloat	= false;
//   protected boolean	unsignedPCM;
}

private[io] object WaveHeader extends AbstractRIFFHeader {
   import AudioFileHeader._ 

   private final val RIFF_MAGIC		= 0x52494646	// 'RIFF'
   private final val WAVE_MAGIC		= 0x57415645	// 'WAVE' (off 8)

// chunk identifiers
   private final val FMT_MAGIC		= 0x666D7420	// 'fmt '
   private final val FACT_MAGIC		= 0x66616374	// 'fact'
   private final val DATA_MAGIC		= 0x64617461	// 'data'
//   private final val CUE_MAGIC		= 0x63756520	// 'cue '
//   private final val SMPL_MAGIC		= 0x73616D6C	// 'smpl'
//   private final val INST_MAGIC		= 0x696E7374	// 'inst'

   // embedded LIST (peak speak) / list (rest of the universe speak) format
//   private final val LIST_MAGIC		= 0x6C697374	// 'list'
//   private final val LIST_MAGIC2	   = 0x4C495354	// 'LIST'

   @throws( classOf[ IOException ])
   def identify( dis: DataInputStream ) = dis.readInt() == RIFF_MAGIC && {
      dis.readInt()
      dis.readInt() == WAVE_MAGIC
   }

   @throws( classOf[ IOException ])
   def read( raf: RandomAccessFile ) : AudioFileHeader = readDataInput( raf )

   @throws( classOf[ IOException ])
   def read( dis: DataInputStream ) : AudioFileHeader = readDataInput( dis )

   @throws( classOf[ IOException ])
   private def readDataInput( din: DataInput ) : AudioFileHeader = {
      if( din.readInt() != RIFF_MAGIC ) formatError()  // RIFF
      din.readInt()
//         len	= raf.len() - 8;
      if( din.readInt() != WAVE_MAGIC ) formatError()	// WAVE
//         len	   -= 4;
      var chunkLen   = 0

      // these we need...
      var afh: AudioFileHeader         = null
      var bpf                          = -1
      var sampleRate                   = -1  // wave uses int
      var sampleFormat: SampleFormat   = null
      var numChannels                  = -1

      try {
         while( afh == null ) {
            if( chunkLen != 0 ) din.skipBytes( chunkLen )  // skip remainder from previous chunk

            val magic   = din.readInt()
            chunkLen	   = (readLittleInt( din ) + 1) & 0xFFFFFFFE
//             len		   -= chunkLen + 8;

            (magic: @switch) match {
               case FMT_MAGIC => {
                  val form          = readLittleUShort( din )  // format
                  numChannels       = readLittleUShort( din )  // # of channels
                  sampleRate        = readLittleInt( din )     // sample rate (integer)
                  val bps				= readLittleInt( din ) 	   // bytes per frame and second (=#chan * #bits/8* rate)
                  bpf               = readLittleUShort( din )	// bytes per frame (=#chan * #bits/8)
                  val bitsPerSample	= readLittleUShort( din )  // # of bits per sample
                  if( ((bitsPerSample & 0x07) != 0) ||
                      ((bitsPerSample >> 3) * numChannels != bpf) ||
                      ((bitsPerSample >>3) * numChannels * sampleRate != bps) ) encodingError()

                  val unsignedPCM	 = bpf == 1 // XXX ??? bpf == numChannels ???
                  chunkLen -= 16

                  val isPCM   = (form: @switch) match {
                     case FORMAT_PCM   => true
                     case FORMAT_FLOAT => false
                     case FORMAT_EXT => {
                        if( chunkLen < 24 ) incompleteError()
                        val i1 = readLittleUShort( din ) // extension size
                        if( i1 < 22 ) incompleteError()
                        val i2 = readLittleUShort( din ) // #valid bits per sample
                        din.readInt()     				   // channel mask, ignore
                        val i3 = readLittleUShort( din ) // GUID first two bytes
                        if(  (i2 != bitsPerSample ) ||
                            ((i3 != FORMAT_PCM) &&
                             (i3 != FORMAT_FLOAT)) ) encodingError()
                        chunkLen -= 10
                        i3 == FORMAT_PCM
                     }
                     case _ => encodingError()
                  }

                  sampleFormat  = if( isPCM ) {
                     (bitsPerSample: @switch) match {
                        case  8 => if( unsignedPCM ) SampleFormat.UInt8 else SampleFormat.Int8
                        case 16 => SampleFormat.Int16
                        case 24 => SampleFormat.Int24
                        case 32 => SampleFormat.Int32
                        case _  => encodingError()
                     }
                  } else {
                     (bitsPerSample: @switch) match {
                        case 32 => SampleFormat.Float
                        case 64 => SampleFormat.Double
                        case _  => encodingError()
                     }
                  }
               }

               case DATA_MAGIC => {
                  if( bpf == -1 ) throw new IOException( "WAVE header misses fmt chunk" )
                  val numFrames  = chunkLen / bpf
                  val spec       = new AudioFileSpec( AudioFileType.Wave, sampleFormat, numChannels, sampleRate,
                     Some( ByteOrder.LITTLE_ENDIAN ), numFrames )
                  afh            = new ReadableAudioFileHeader( spec, ByteOrder.LITTLE_ENDIAN )
               }

               case _ => // ignore unknown chunks
            } // magic match
         } // essentials loop
      } catch {
         case e: EOFException =>
      }

      if( afh == null ) throw new IOException( "WAVE header misses data chunk" )
      afh
   }

   @throws( classOf[ IOException ])
   def write( raf: RandomAccessFile, spec: AudioFileSpec ) : WritableAudioFileHeader = {
      val (_, _, spec1) = writeDataOutput( raf, spec )
      new NonUpdatingWritableHeader( spec1 )
   }

   @throws( classOf[ IOException ])
   def write( dos: DataOutputStream, spec: AudioFileSpec ) : WritableAudioFileHeader = {
      val (factSmpNumOffset, dataLengthOffset, spec1) = writeDataOutput( dos, spec )
      sys.error( "TODO" ) // XXX new NonUpdatingWritableHeader( spec1 )
   }

   @throws( classOf[ IOException ])
   private def writeDataOutput( dout: DataOutput, spec: AudioFileSpec ) : (Long, Long, AudioFileSpec) = {
      val res = spec.byteOrder match {
         case Some( ByteOrder.LITTLE_ENDIAN )   => spec
         case None                              => spec.copy( byteOrder = Some( ByteOrder.LITTLE_ENDIAN ))
         case Some( other )                     => throw new IOException( "Unsupported byte order " + other )
      }

      val ??? = sys.error( "TODO" )

      dout.writeInt( RIFF_MAGIC )
      dout.writeInt( ??? ) // 0  // length except RIFF-Header (file length minus 8)
      dout.writeInt( WAVE_MAGIC )

      var pos = 12L

      // floating point requires FACT extension
      val isFloat = spec.sampleFormat == SampleFormat.Float || spec.sampleFormat == SampleFormat.Double

      // fmt Chunk
      val intRate       = (spec.sampleRate + 0.5).toInt
      val bitsPerSample = spec.sampleFormat.bitsPerSample
      val frameSize     = (bitsPerSample >> 3) * spec.numChannels

      val fmtSize = if( isFloat ) 26 else 24   // FORMAT_FLOAT has extension of size 2
      dout.writeInt( FMT_MAGIC )
      writeLittleInt( dout, fmtSize - 8 )
      writeLittleShort( dout, if( isFloat ) FORMAT_FLOAT else FORMAT_PCM )
      writeLittleShort( dout, spec.numChannels )
      writeLittleInt( dout, intRate )
      writeLittleInt( dout, intRate * frameSize )
      writeLittleShort( dout, frameSize )
      writeLittleShort( dout, bitsPerSample )
      if( isFloat ) dout.writeShort( 0 )

      pos += fmtSize

      // fact Chunk
      val factSmpNumOffset = if( isFloat ) {
         dout.writeInt( FACT_MAGIC )
         writeLittleInt( dout, 4 )
         dout.writeInt( ??? ) // 0
         pos += 12
         pos - 4
      } else 0L

//      // cue Chunk
//      val markers  = Vector.empty // (List) descr.getProperty( AudioFileInfo.KEY_MARKERS );
//      val regions  = Vector.empty // (List) descr.getProperty( AudioFileInfo.KEY_REGIONS );
//      if( markers.nonEmpty) || regions.nonEmpty ) {
//         raf.writeInt( CUE_MAGIC );
//         i2	= markers.size() + regions.size();
//         writeLittleInt( 24 * i2 + 4 );
//         writeLittleInt( i2 );
//         for( i= 0, i1 = 1; i < markers.size(); i++, i1++ ) {
//            val marker = (Marker) markers.get( i );
//            writeLittleInt( i1 );
//            writeLittleInt( i1 );
//            raf.writeInt( DATA_MAGIC );
//            raf.writeLong( 0 );	// ignore dwChunkStart, dwBlockStart
//            writeLittleInt( (int) marker.pos );
//         }
//         for( i = 0; i < regions.size(); i++, i1++ ) {
//            region = (Region) regions.get( i );
//            writeLittleInt( i1 );
//            writeLittleInt( i1 );
//            raf.writeInt( DATA_MAGIC );
//            raf.writeLong( 0 );	// ignore dwChunkStart, dwBlockStart
//            writeLittleInt( (int) region.span.getStart() );
//         }
//
//         raf.writeInt( LIST_MAGIC );
//         pos	= raf.getFilePointer();
//         raf.writeInt( 0);
//         raf.writeInt( ADTL_MAGIC );
//
//         for( i = 0, i1= 1; i < markers.size(); i++, i1++ ) {
//            marker	= (Marker) markers.get( i );
//            i3		= marker.name.len() + 5;
//            raf.writeInt( LABL_MAGIC );
//            writeLittleInt( i3 );
//            writeLittleInt( i1 );
//            raf.writeBytes( marker.name );
//            if( (i3 & 1) == 0 ) raf.writeByte( 0 ); else raf.writeShort( 0 );
//         }
//
//         for( i= 0; i < regions.size(); i++, i1++ ) {
//            region	= (Region) regions.get( i );
//            i3	= region.name.len() + 5;
//            raf.writeInt( LABL_MAGIC );
//            writeLittleInt( i3 );
//            writeLittleInt( i1 );
//            raf.writeBytes( region.name );
//            if((i3 & 1) == 0 ) raf.writeByte( 0); else raf.writeShort(0 );
//         }
//
//         for( i = 0, i1 = markers.size() + 1; i < regions.size(); i++, i1++ ) {
//            val region	= (Region) regions.get( i );
//            raf.writeInt( LTXT_MAGIC );
//            writeLittleInt( 21 );
//            writeLittleInt( i1 );
//            writeLittleInt( (int) region.span.getLength() );
//            raf.writeInt( RGN_MAGIC);
//            raf.writeLong( 0 );		// wCountry, wLanguage, wDialect, wCodePage
//            raf.writeShort( 0 );	// no name (already specified in 'labl' chunk (zero + pad)
//         }
//
//         // update 'list' chunk size
//         pos2 = raf.getFilePointer();
//         i	 = (int)(pos2 - pos - 4);
//         if( (i & 1) == 1 ) {
//            raf.writer( 0 );	// padding byte
//            pos2++;
//         }
//         raf.seek( pos );
//         writeLittleInt( i );
//         raf.seek( pos2 );
//
//      } // if marker or region list not empty

//      // smpl Chunk
//      val loopRegion = (Region) descr.getProperty( AudioFileInfo.KEY_LOOP );
//      if( loopRegion != null ){
//         raf.writeInt( SMPL_MAGIC );
//         writeLittleInt( 36 + 24 );
//         raf.writeLong( 0 );		// dwManufacturer,dwProduct
//         writeLittleInt( (int) (1.0e9 / descr.rate + 0.5) );	// dwSamplePeriod
//         writeLittleInt( 69 );	// dwMIDIUnityNote
//         raf.writeInt( 0 );		// dwMIDIPitchFraction
//         raf.writeLong( 0 );		// dwSMPTEFormat, dwSMPTEOffset
//         writeLittleInt( 1 );	// just one loop
//         raf.writeInt( 0 );		// no additional chunkinformation
//
//         writeLittleInt( 0 );	// loop gets ID 0
//         writeLittleInt( 0 );	// normal loop
//         writeLittleInt( (int) loopRegion.span.getStart() );
//         writeLittleInt( (int) loopRegion.span.getStop() );
//         raf.writeLong( 0 );		// dwFraction, dwPlayCount
//      }

//      // inst Chunk
//      val o = descr.getProperty( AudioFileInfo.KEY_GAIN );
//      if( o != null ) {
//         i1	= Math.max( -64, Math.min( 63, (int) (20 * Math.log( ((Float) o).floatValue() ) / Math.log( 10 ) + 0.5) ));
//         raf.writeInt( INST_MAGIC );
//         writeLittleInt( 7 );
//         raf.writeShort( (69 << 24) | (0 << 16) );	//char: MIDI Note, Detune
//         raf.writer( i1 );							// char gain (dB)
//         raf.writeInt( 0x007F007F );					// char LowNote, HighNote, velLo, char velHi
//         raf.writer( 0 );								// pad byte
//      }

      // data Chunk (Header)
      dout.writeInt( DATA_MAGIC )
//      dataLengthOffset = raf.getFilePointer()
      dout.writeInt( ??? ) // 0
      pos += 8

      val dataLengthOffset = pos - 4

      (factSmpNumOffset, dataLengthOffset, res)
   }
}

//
//   protected void updateHeader( AudioFileInfo descr )
//   throws IOException
//   {
//long oldPos	= raf.getFilePointer();
//      long len	= raf.len();
//      if( len == lastUpdateLength ) return;
//
//      if( len >= riffLengthOffset + 4 ) {
//         raf.seek( riffLengthOffset );
//         writeLittleInt( (int) (len - 8) );								// RIFF Chunk len
//      }
//      if( isFloat &&(len >= factSmpNumOffset + 4) ) {
//         raf.seek( factSmpNumOffset );
//         writeLittleInt( (int) (descr.len * descr.channels) );			// fact: Sample-Num XXX check multich.!
//      }
//      if( len >= dataLengthOffset + 4 ) {
//         raf.seek(dataLengthOffset );
//         writeLittleInt( (int) (len - (dataLengthOffset + 4)) );			// data Chunk len
//      }
//      raf.seek( oldPos );
//      lastUpdateLength= len;
//   }
//

/*
// http://www.vcs.de/fileadmin/user_upload/MBS/PDF/Whitepaper/Informations_about_Sony_Wave64.pdf
private class Wave64Header
extends AbstractRIFFHeader
{
   private static final int RIFF_MAGIC1a	= 0x72696666;	// 'riff'
   private staticfinal int RIFF_MAGIC1b	= 0x2E91CF11;
   private static final long RIFF_MAGIC1	= 0x726966662E91CF11L;
   private static final long RIFF_MAGIC2	= 0xA5D628DB04C10000L;

   private static final long WAVE_MAGIC1	= 0x77617665F3ACD311L;	// 'wave'-XXXX
   private static final long WAVE_MAGIC2	= 0x8CD100C04F8EDB8AL;

   // chunk identifiers
private static final long FMT_MAGIC1	= 0x666D7420F3ACD311L;	// 'fmt '-XXXX
   private static final long FMT_MAGIC2	= 0x8CD100C04F8EDB8AL;
   private static final long FACT_MAGIC1	= 0x66616374F3ACD311L;	// 'fact'-XXXX
   private static final long FACT_MAGIC2	= 0x8CD100C04F8EDB8AL;
   private static final long DATA_MAGIC1	= 0x64617461F3ACD311L;	// 'data'-XXXX
   private static final long DATA_MAGIC2	= 0x8CD100C04F8EDB8AL;

//		private static final long LIST_MAGIC1	= 0x6C6973742F91CF11L; // 'list'-XXXX
//		private static final longLIST_MAGIC2	= 0xA5D628DB04C10000L;
   private static final long MARKER_MAGIC1	= 0x5662F7AB2D39D211L;
   private static final long MARKER_MAGIC2	= 0x86C700C04F8EDB8AL;

   private final Charset charset = Charset.forName( "UTF-16LE" );
   private long		markersOffset			= 0L;
   private static final long riffLengthOffset	= 16L;

   protected Wave64Header() {  }

   protected void readHeader( AudioFileInfo descr )
   throws IOException
   {
      int		i, i1, i2, i3, essentials, bpf = 0;
      long	len, magic1, magic2, chunkLen, dataLen = 0;

      raf.readLong(); raf.readLong();		// riff
      len	= readLittleLong();
      raf.readLong(); raf.readLong();// wave
      len	   -= 40;
      chunkLen = 0;

//System.out.println( "len = " + len );

      for( essentials = 2; (len >= 24) && (essentials > 0); ){
         if( chunkLen != 0 ) raf.seek( raf.getFilePointer() + chunkLen );	// skip to next chunk

         magic1		= raf.readLong();
magic2		= raf.readLong();
         chunkLen	= (readLittleLong() + 7) & 0xFFFFFFFFFFFFFFF8L;

//System.out.println( "magic1 = " + magic1 +"; chunkLen = " + chunkLen + "; pos = " + raf.getFilePointer() );

         len		   -= chunkLen;
         chunkLen   -= 24;

         if( magic1 == FMT_MAGIC1 && magic2 == FMT_MAGIC2 ) {
            essentials--;
            i					= readLittleUShort();		// format
            descr.channels	= readLittleUShort();		// # of channels
            i1= readLittleInt();			// sample rate (integer)
descr.rate			= i1;
            i2	= readLittleInt();			// bytes per frame and second (=#chan * #bits/8 * rate)
bpf		= readLittleUShort();		// bytes per frame(=#chan * #bits/8)
            descr.bitsPerSample	= readLittleUShort();		// # of bits per sample
            if( ((descr.bitsPerSample & 0x07) != 0) ||
               ((descr.bitsPerSample >> 3) * descr.channels != bpf) ||
               ((descr.bitsPerSample >> 3) * descr.channels * i1 != i2) ) {

               throw new IOException( getResourceString( "errAudioFileEncoding" ));
            }
            unsignedPCM			= bpf == 1;

            chunkLen -=16;

            switch( i ) {
      case FORMAT_PCM:
   descr.sampleFormat = AudioFileInfo.FORMAT_INT;
               break;
            case FORMAT_FLOAT:
               descr.sampleFormat = AudioFileInfo.FORMAT_FLOAT;
               break;
            case FORMAT_EXT:
               if( chunkLen < 24 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));
               i1 = readLittleUShort();	// extension size
               if( i1 < 22 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));
               i2 = readLittleUShort();	// # valid bits per sample
               raf.readInt();				// channel mask, ignore
               i3 = readLittleUShort();	// GUID first two bytes
               if( (i2 != descr.bitsPerSample) ||
                  ((i3 != FORMAT_PCM) &&
                  (i3 != FORMAT_FLOAT)) ) throw new IOException( getResourceString( "errAudioFileEncoding" ));
               descr.sampleFormat =i3 == FORMAT_PCM ? AudioFileInfo.FORMAT_INT : AudioFileInfo.FORMAT_FLOAT;
               chunkLen -= 10;
               break;
            default:
               throw new IOException( getResourceString( "errAudioFileEncoding" ));
            }

         } else if( magic1 == DATA_MAGIC1 && magic2 == DATA_MAGIC2 ) {
            essentials--;
            sampleDataOffset	= raf.getFilePointer();
            dataLen				= chunkLen;

         } else if( magic1 == MARKER_MAGIC1 && magic2 == MARKER_MAGIC2 ) {
            markersOffset			= raf.getFilePointer();
         }
      } // for( essentials = 2; (len > 0) && (essentials > 0); )
      if( essentials > 0 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));

      descr.len = dataLen / bpf;
   }

   protected void writeHeader( AudioFileInfo descr)
   throws IOException
   {
      final List		markers, regions;
      int				i1, i2;
      String			str;
      Region			region;
      Marker			marker;
      long			pos, pos2, n1, n2;

      isFloat = descr.sampleFormat == AudioFileInfo.FORMAT_FLOAT;	// floating point requires FACT extension
      raf.writeLong( RIFF_MAGIC1 );
      raf.writeLong( RIFF_MAGIC2 );
      raf.writeLong( 40 );		// Laenge inkl. RIFF-Header (Dateilaenge); unknown now
      raf.writeLong( WAVE_MAGIC1 );
      raf.writeLong( WAVE_MAGIC2 );

      // ---- fmt Chunk----
      raf.writeLong( FMT_MAGIC1 );
      raf.writeLong( FMT_MAGIC2 );
writeLittleLong( isFloat ? 42 : 40 );  // FORMAT_FLOAT has extension of size 0
      writeLittleShort( isFloat ? FORMAT_FLOAT : FORMAT_PCM );
      writeLittleShort( descr.channels );
      i1 = (int) (descr.rate + 0.5);
   writeLittleInt( i1 );
      i2 = (descr.bitsPerSample >> 3) * descr.channels;
      writeLittleInt( i1 * i2 );
      writeLittleShort( i2 );
      writeLittleShort( descr.bitsPerSample );

      if( isFloat ) {
//raf.writeShort( 0 );
raf.writeLong( 0 ); // actually a short, but six extra bytes to align to 8-byte boundary
      }

      // ---- fact Chunk ----
      if( isFloat ) {
         raf.writeLong( FACT_MAGIC1 );
         raf.writeLong( FACT_MAGIC2 );
         writeLittleLong( 32 );
         factSmpNumOffset = raf.getFilePointer();
//			raf.writeInt( 0 );
         raf.writeLong( 0 ); // i guess it shouldbe long???
      }

      // -- marker Chunk ----
      markers  = (List) descr.getProperty( AudioFileInfo.KEY_MARKERS );
      regions  = (List) descr.getProperty( AudioFileInfo.KEY_REGIONS );
      if( ((markers != null) && !markers.isEmpty()) || ((regions != null) && !regions.isEmpty()) ) {

         final CharsetEncoder	enc		= charset.newEncoder();
         CharBuffer				cbuf	= null;
         ByteBuffer				bbuf	= null;
         final List[] cues = {
            markers == null ? Collections.EMPTY_LIST : markers,
         regions == null? Collections.EMPTY_LIST : regions
         };

         raf.writeLong( MARKER_MAGIC1 );
         raf.writeLong( MARKER_MAGIC2 );
   pos	= raf.getFilePointer();
         raf.writeLong( 0 );// updated afterwards
         i2	= cues[ 0 ].size() + cues[ 1 ].size();
      writeLittleInt( i2 );
// CUE64 structures
         for( int i = 0, id = 1; i < 2; i++ ) {
            for( int j = 0; j < cues[ i ].size(); j++, id++ ) {
               if( i == 0) {
                  marker	= (Marker) cues[ i ].get( j );
                  n1		= marker.pos;
                  n2		= -1;
str		= marker.name;
               } else {
                  region = (Region) cues[ i ].get( j);
                  n1		= region.span.start;
                  n2		= region.span.getLength();
                  str		= region.name;
   }
               writeLittleInt( id );	// marker ID
               raf.writeInt( 0 );		// padding
               writeLittleLong( n1 );	// position
               writeLittleLong(n2 );	// len

               if( (cbuf == null) || (cbuf.capacity() < str.len()) ) {
                  cbuf = CharBuffer.allocate( str.len() + 8 );
                  bbuf = ByteBuffer.allocate( (cbuf.capacity() + 1) << 1 );
               }
               cbuf.clear();
               cbuf.put( str );
               cbuf.flip();
               bbuf.clear();
               enc.reset();
enc.encode( cbuf, bbuf, true );
               enc.flush( bbuf);
               bbuf.putShort( (short) 0 ); // null term
               bbuf.flip();

               writeLittleInt( bbuf.remaining() );
   raf.writeInt( 0 );		// padding
//System.out.println( "writing " + bbuf.remaining() + " bytes at " + fch.position() );
               fch.writer( bbuf );
            }
         }

         // update chunk size
         pos2 = raf.getFilePointer();
         n1	 = pos2 - pos;
//System.out.println( "n1 = " +n1 + "; pos = " + pos + "; pos2 = " + pos2 + "; pad = " + (int) (((n1 + 7) & 0xFFFFFFFFFFFFFFF8L) - n1) );
         final int pad = (int) (((n1 + 7) & 0xFFFFFFFFFFFFFFF8L)- n1);
      for( int i = 0; i < pad; i++ ) raf.writer( 0 );	// padding byte

         raf.seek( pos );
         writeLittleLong( n1 + 16);
//				writeLittleLong( n1 + 16 + pad );
         raf.seek( pos2 + pad );

      } // if marker orregion list not empty

   // data Chunk (Header)
      raf.writeLong( DATA_MAGIC1 );
   raf.writeLong( DATA_MAGIC2 );
      dataLengthOffset = raf.getFilePointer();
      raf.writeLong( 24 );
      sampleDataOffset = raf.getFilePointer();

      updateHeader( descr );
   }

   protected void updateHeader( AudioFileInfo descr )
   throws IOException
   {
      final long oldPos	= raf.getFilePointer();
      final long len		= raf.len();
if( len == lastUpdateLength ) return;
      final long lenM8	= len- 8;

      if( lenM8 >= riffLengthOffset ) {
         raf.seek( riffLengthOffset );
// System.out.println( "updateHeader: len = " + len );
         writeLittleLong( len );		// riff Chunk len
      }
      if( isFloat && (lenM8 >= factSmpNumOffset) ) {
         raf.seek( factSmpNumOffset );
         writeLittleLong( descr.len * descr.channels);			// fact: Sample-Num XXX check multich.!
      }
      if( lenM8 >= dataLengthOffset ) {
         raf.seek(dataLengthOffset );
         writeLittleLong( len - (dataLengthOffset - 16) );	// data Chunk len
      }
      raf.seek( oldPos );
      lastUpdateLength =len;
   }

   protected long getSampleDataOffset()
   {
      return sampleDataOffset;
   }

   protected ByteOrder getByteOrder()
   {
      return ByteOrder.LITTLE_ENDIAN;
   }

   protected boolean isUnsignedPCM()
   {
      return unsignedPCM;
   }

   protected void readMarkers()
throws IOException
   {
//System.out.println( "markersOffset = " + markersOffset );
      if( markersOffset == 0L ) return;

      final List				markers	= new ArrayList();
      final List	regions	= new ArrayList();
      final CharsetDecoder	dec		= charset.newDecoder();
      CharBuffer				cbuf	=null;
      ByteBuffer				bbuf	= null;
      long					n1, n2;
      int						numBytes;
      String		str;
      CoderResult				result;

      final long oldPos = raf.getFilePointer();
      try {
         raf.seek( markersOffset );
         for( int numCues = readLittleInt(), cue = 0; cue< numCues; cue++ ) {
//System.out.println( "cue " + (cue+1) + " of " + numCues );
            raf.readInt();					// marker ID (ignore)
   raf.readInt(); 					// padding
            n1			= readLittleLong();	// pos
            n2		= readLittleLong();	// len (-1 for markers)
            numBytes	= readLittleInt();	// size of name string in bytes
            raf.readInt(); 					// padding

      if( bbuf == null || bbuf.capacity() < numBytes ) {
      bbuf = ByteBuffer.allocate( numBytes + 16 );
               cbuf = CharBuffer.allocate( bbuf.capacity() >> 1 );
      }

            bbuf.rewind().limit( numBytes);

//System.out.println( "reading " + bbuf.remaining() + " bytes from " + fch.position() );

            fch.reader( bbuf );
         if( (numBytes >= 2) &&
    (bbuf.get( numBytes - 2 ) == 0) &&
(bbuf.get( numBytes -1 ) == 0) ) { // null term

               bbuf.rewind().limit( numBytes - 2 );
      } else {
               bbuf.flip();
            }
cbuf.clear();
            dec.reset();
            result = dec.decode( bbuf, cbuf, true );
            if( result.isError() ) {
               throw new IOException( "Error Reading Cue Name" +
                  (result.isMalformed() ? ": Malformed Input" :
               (result.isOverflow() ? ": Overflow" :
                  (result.isUnderflow() ? ": Underflow" :
                  (result.isUnmappable() ? ": Unmappable" : "")))));
}
            dec.flush( cbuf );
            cbuf.flip();
   str = cbuf.toString();

// System.out.println( "n1 = " + n1 + "; n2 = " + n2 + "; name  = '" + str + "'" );

            if( n2 < 0 ) {
               markers.add( new Marker( n1, str ));
            } else {
               regions.add( new Region( new Span( n1, n1 + n2 ), str ));
            }
         }

         afd.setProperty( AudioFileInfo.KEY_MARKERS, markers );
         afd.setProperty( AudioFileInfo.KEY_REGIONS, regions );

      }
      finally {
         raf.seek( oldPos );
      }
   }
} // class Wave64Header
*/
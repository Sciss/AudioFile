package de.sciss.synth.io.impl

import de.sciss.synth.io.{WritableAudioFileHeader, AudioFileSpec, AudioFileType, AudioFileHeader}
import java.io.{DataOutput, RandomAccessFile, EOFException, DataInputStream, IOException, DataInput}
import java.nio.ByteOrder

/**
 * The 64 bit version of Wave.
 * see http://www.vcs.de/fileadmin/user_upload/MBS/PDF/Whitepaper/Informations_about_Sony_Wave64.pdf
 * (link broken; now: http://www.ambisonia.com/Members/mleese/sony_wave64.pdf )
 */
private[io] object Wave64Header extends AbstractRIFFHeader {
   import AudioFileHeader._
//   private final val RIFF_MAGIC1a	= 0x72696666	// 'riff' ; first 4 bytes of RIFF-GUID in big endian notation
//   private final val RIFF_MAGIC1b	= 0x2E91CF11   //          next 2 shorts of RIFF-GUID in big endian notation

   private final val RIFF_MAGIC1 = 0x726966662E91CF11L   // RIFF-GUID in big endian notation
   private final val RIFF_MAGIC2 = 0xA5D628DB04C10000L   // ...

   private final val WAVE_MAGIC1 = 0x77617665F3ACD311L   // WAVE-GUID in big endian notation
   private final val WAVE_MAGIC2 = 0x8CD100C04F8EDB8AL   // ...

   // chunk identifiers
   private final val FMT_MAGIC1  = 0x666D7420F3ACD311L   // 'fmt ' GUID in big endian notation
   private final val FMT_MAGIC2  = 0x8CD100C04F8EDB8AL

   private final val DATA_MAGIC1 = 0x64617461F3ACD311L   // 'data' GUID in big endian notation
   private final val DATA_MAGIC2 = 0x8CD100C04F8EDB8AL

   private final val FACT_MAGIC1 = 0x66616374F3ACD311L   // 'fact' GUID in big endian notation
   private final val FACT_MAGIC2 = 0x8CD100C04F8EDB8AL

   @throws( classOf[ IOException ])
   def identify( dis: DataInputStream ) = dis.readLong() == RIFF_MAGIC1 && dis.readLong() == RIFF_MAGIC2 && {
      dis.skipBytes( 8 )   // length
      dis.readLong() == WAVE_MAGIC1 && dis.readLong() == WAVE_MAGIC2
   }

   @throws( classOf[ IOException ])
   protected def readDataInput( din: DataInput ) : AudioFileHeader = {
      if( din.readLong() != RIFF_MAGIC1 || din.readLong() != RIFF_MAGIC2 ) formatError()  // RIFF
      din.skipBytes( 8 )   // len = readLittleLong
      if( din.readLong() != WAVE_MAGIC1 || din.readLong() != WAVE_MAGIC2 ) formatError()  // WAVE

      var chunkLen         = 0L
      var fc: FormatChunk  = null

      try {
         while( true ) {
            while( chunkLen > 0 ) {
               val skp = math.min( chunkLen, 0x7FFFFFFF ).toInt
               din.skipBytes( skp )
               chunkLen -= skp
            }

            val magic1  = din.readLong()
            val magic2  = din.readLong()
            chunkLen	   = (readLittleLong( din ) + 7) & 0xFFFFFFFFFFFFFFF8L

//          len         -= chunkLen
            chunkLen   -= 24

            if( magic1 == FMT_MAGIC1 && magic2 == FMT_MAGIC2 ) {
               fc = readFormatChunk( din, chunkLen.toInt )
               chunkLen -= fc.chunkSkip

            } else if( magic1 == DATA_MAGIC1 && magic2 == DATA_MAGIC2 ) {
               return createReader( fc, AudioFileType.Wave64, chunkLen )

//            } else if( magic1 == MARKER_MAGIC1 && magic2 == MARKER_MAGIC2 ) {
//               markersOffset			= raf.getFilePointer()
            }
         }
      } catch {
         case _: EOFException =>
      }
      throw new IOException( AudioFileType.Wave64.name + " header misses data chunk" )
   }

   final protected def createWriter( raf: RandomAccessFile, spec: AudioFileSpec, factSmpNumOffset: Long,
                                     dataChunkLenOff : Long ) : WritableAudioFileHeader =
      new WritableFileHeader( raf, spec, factSmpNumOffset = factSmpNumOffset, dataChunkLenOff = dataChunkLenOff )

   final protected def writeRiffMagic( dout: DataOutput, fileSize: Long ) {
      dout.writeLong( RIFF_MAGIC1 )
      dout.writeLong( RIFF_MAGIC2 )
      dout.writeLong( fileSize )         // total size (including cookie chunk header!)
      dout.writeLong( WAVE_MAGIC1 )
      dout.writeLong( WAVE_MAGIC2 )
   }

   final protected def writeFmtMagic( dout: DataOutput, fmtChunkSize: Int ) {
      dout.writeLong( FMT_MAGIC1 )
      dout.writeLong( FMT_MAGIC2 )
      writeLittleLong( dout, fmtChunkSize )
   }

   final protected def writeFactChunk( dout: DataOutput, numFrames: Long ) {
      dout.writeLong( FACT_MAGIC1 )
      dout.writeLong( FACT_MAGIC2 )
      writeLittleLong( dout, 32 )
      dout.writeLong( numFrames )
   }

   final protected def writeDataMagic( dout: DataOutput, dataChunkSize: Long ) {
      dout.writeLong( DATA_MAGIC1 )
      dout.writeLong( DATA_MAGIC2 )
      writeLittleLong( dout, dataChunkSize )
   }

   final protected val cookieSize: Int    = 16
   final protected val chunkLenSize: Int  = 8
   final protected val chunkPad: Int      = 8

   final private class WritableFileHeader( raf: RandomAccessFile, val spec: AudioFileSpec,
                                           factSmpNumOffset: Long, dataChunkLenOff: Long )
   extends WritableAudioFileHeader {
      private var numFrames0 = 0L

      @throws( classOf[ IOException ])
      def update( numFrames: Long ) {
         if( numFrames == numFrames0 ) return

         val oldPos	   = raf.getFilePointer
         val fileSize   = raf.length()
         raf.seek( cookieSize )
         writeLittleLong( raf, fileSize )

         if( factSmpNumOffset != 0L ) {
            raf.seek( factSmpNumOffset )
            writeLittleLong( raf, numFrames )
         }

         raf.seek( dataChunkLenOff )
         val dataChunkSize = fileSize - (dataChunkLenOff - cookieSize)
         writeLittleLong( raf, dataChunkSize )

         raf.seek( oldPos )
         numFrames0 = numFrames
      }

      def byteOrder : ByteOrder = spec.byteOrder.get
   }
}
/*
private class Wave64Header
extends AbstractRIFFHeader
{
   private static final int RIFF_MAGIC1a	= 0x72696666;	// 'riff'
   private staticfinal int RIFF_MAGIC1b	= 0x2E91CF11;
   private static final long RIFF_MAGIC1	= 0x726966662E91CF11L;
   private static final long RIFF_MAGIC2	= 0xA5D628DB04C10000L;

   private static final long WAVE_MAGIC1	= 0x77617665F3ACD311L;	// 'wave'
   private static final long WAVE_MAGIC2	= 0x8CD100C04F8EDB8AL;

   // chunk identifiers
private static final long FMT_MAGIC1	= 0x666D7420F3ACD311L;	// 'fmt '
   private static final long FMT_MAGIC2	= 0x8CD100C04F8EDB8AL;
   private static final long FACT_MAGIC1	= 0x66616374F3ACD311L;	// 'fact'
   private static final long FACT_MAGIC2	= 0x8CD100C04F8EDB8AL;
   private static final long DATA_MAGIC1	= 0x64617461F3ACD311L;	// 'data'
   private static final long DATA_MAGIC2	= 0x8CD100C04F8EDB8AL;

//		private static final long LIST_MAGIC1	= 0x6C6973742F91CF11L; // 'list'
//		private static final longLIST_MAGIC2	= 0xA5D628DB04C10000L;
   private static final long MARKER_MAGIC1	= 0x5662F7AB2D39D211L;
   private static final long MARKER_MAGIC2	= 0x86C700C04F8EDB8AL;

   private final Charset charset = Charset.forName( "UTF-16LE" );
   private long		markersOffset			= 0L;
   private static final long riffLengthOffset	= 16L;

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
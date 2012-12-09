package de.sciss.synth.io
package impl

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
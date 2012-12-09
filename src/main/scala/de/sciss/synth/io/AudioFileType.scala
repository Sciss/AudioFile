/*
 *  AudioFileType.scala
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

import collection.immutable.{IndexedSeq => IIdxSeq}
import java.io.{File, InputStream, DataOutputStream, RandomAccessFile, DataInputStream, IOException}

/**
 * A recognized audio file type.
 */
sealed trait AudioFileType {
   /**
    * @return  the unique identifier of the file type
    */
   def id: String

   /**
    * @return  the default extension (period not included)
    */
   def extension: String

   /**
    * @return  a list of alternative extensions, including the default `extension`
    */
   def extensions: IIdxSeq[ String ]
}

object AudioFileType {
   sealed trait CanIdentify extends AudioFileType {
      @throws( classOf[ IOException ])
      private[io] def identify( dis: DataInputStream ) : Boolean
   }
   trait CanRead extends AudioFileType {
      @throws( classOf[ IOException ])
      private[io] def read( dis: DataInputStream ) : AudioFileHeader

      @throws( classOf[ IOException ])
      private[io] def read( raf: RandomAccessFile ) : AudioFileHeader
   }
   trait CanWrite extends AudioFileType {
      @throws( classOf[ IOException ])
      private[io] def write( dos: DataOutputStream, spec: AudioFileSpec ) : WritableAudioFileHeader

      @throws( classOf[ IOException ])
      private[io] def write( raf: RandomAccessFile, spec: AudioFileSpec ) : WritableAudioFileHeader
   }

//   private val sync  = new AnyRef
   private var allVar      = IIdxSeq.empty[ AudioFileType ]
   private var knownVar    = IIdxSeq.empty[ CanIdentify ]
   private var readableVar = IIdxSeq.empty[ CanRead ]
   private var writableVar = IIdxSeq.empty[ CanWrite ]

   /**
    * Restores a file type from its identifier.
    * Throws a runtime exception if the identifier is unknown.
    */
   def apply( id: String ) : AudioFileType = allVar.find( _.id == id ).getOrElse(
      throw new IllegalArgumentException( "Unknown identifier " + id ))

   private[io] def register( fileType: AudioFileType ) {
      allVar :+= fileType

      fileType match {
         case ci: CanIdentify => knownVar :+= ci
         case _ =>
      }
      fileType match {
         case cr: CanRead => readableVar :+= cr
         case _ =>
      }
      fileType match {
         case cw: CanWrite => writableVar :+= cw
         case _ =>
      }
   }

   register( AIFF   )
   register( Wave   )
   register( Wave64 )
   register( NeXT   )
   register( IRCAM  )
   register( Raw    )

   /**
    *    Note: this Iterator does not include the Raw type
    *    which usually requires special handling.
    */
   def known    : IIdxSeq[ CanIdentify ]  = knownVar
   def readable : IIdxSeq[ CanRead ]      = readableVar
   def writable : IIdxSeq[ CanWrite ]     = writableVar

   /**
    * Apple's audio interchange file format
    */
   case object AIFF extends CanIdentify with CanRead with CanWrite {
      import impl.{AIFFHeader => Impl}

      final val id         = "aiff"
      final val extension  = "aif"
      final val extensions = IIdxSeq( "aif", "aiff", "aifc" )

      private[io] def identify( dis: DataInputStream  ) : Boolean          = Impl.identify( dis )
      private[io] def read(     dis: DataInputStream  ) : AudioFileHeader  = Impl.read( dis )
      private[io] def read(     raf: RandomAccessFile ) : AudioFileHeader  = Impl.read( raf )
      private[io] def write(    dos: DataOutputStream, spec: AudioFileSpec) : WritableAudioFileHeader = Impl.write( dos, spec )
      private[io] def write(    raf: RandomAccessFile, spec: AudioFileSpec) : WritableAudioFileHeader = Impl.write( raf, spec )
   }

   /**
    * The NeXT .snd or Sun .au format
    */
   case object NeXT extends CanIdentify with CanRead /* with CanWrite XXX */ {
      import impl.{NeXTHeader => Impl}

      final val id         = "next"
      final val extension  = "au"
      final val extensions = IIdxSeq( "au", "snd" )

      private[io] def identify( dis: DataInputStream  ) : Boolean          = Impl.identify( dis )
      private[io] def read(     dis: DataInputStream  ) : AudioFileHeader  = Impl.read( dis )
      private[io] def read(     raf: RandomAccessFile ) : AudioFileHeader  = Impl.read( raf )
//      private[io] def write(    dos: DataOutputStream, spec: AudioFileSpec) : WritableAudioFileHeader = Impl.write( dos, spec )
//      private[io] def write(    raf: RandomAccessFile, spec: AudioFileSpec) : WritableAudioFileHeader = Impl.write( raf, spec )
   }

   /**
    * Microsoft's Wave (RIFF) format
    */
   case object Wave extends CanIdentify with CanRead /* with CanWrite XXX */ {
      import impl.{WaveHeader => Impl}

      final val id         = "wav"
      final val extension  = "wav"
      final val extensions = IIdxSeq( "wav", "wave" )

      private[io] def identify( dis: DataInputStream  ) : Boolean          = Impl.identify( dis )
      private[io] def read(     dis: DataInputStream  ) : AudioFileHeader  = Impl.read( dis )
      private[io] def read(     raf: RandomAccessFile ) : AudioFileHeader  = Impl.read( raf )
//      private[io] def write(    dos: DataOutputStream, spec: AudioFileSpec) : WritableAudioFileHeader = Impl.write( dos, spec )
//      private[io] def write(    raf: RandomAccessFile, spec: AudioFileSpec) : WritableAudioFileHeader = Impl.write( raf, spec )
   }

   /**
    * IRCAM, Berkeley or Carl sound format (BICSF)
    */
   case object IRCAM extends CanIdentify with CanRead with CanWrite {
      import impl.{IRCAMHeader => Impl}

      final val id         = "ircam"
      final val extension  = "sf"
      final val extensions = IIdxSeq( "sf", "irc" )

      private[io] def identify( dis: DataInputStream  ) : Boolean          = Impl.identify( dis )
      private[io] def read(     dis: DataInputStream  ) : AudioFileHeader  = Impl.read( dis )
      private[io] def read(     raf: RandomAccessFile ) : AudioFileHeader  = Impl.read( raf )
      private[io] def write(    dos: DataOutputStream, spec: AudioFileSpec) : WritableAudioFileHeader = Impl.write( dos, spec )
      private[io] def write(    raf: RandomAccessFile, spec: AudioFileSpec) : WritableAudioFileHeader = Impl.write( raf, spec )
   }

   /**
    * Raw (headerless) file type
    */
   case object Raw extends CanWrite {
      import impl.{RawHeader => Impl}

      final val id         = "raw"
      final val extension  = "raw"
      final val extensions = IIdxSeq( "raw" )

      private[io] def write( dos: DataOutputStream, spec: AudioFileSpec) : WritableAudioFileHeader = Impl.write( dos, spec )
      private[io] def write( raf: RandomAccessFile, spec: AudioFileSpec) : WritableAudioFileHeader = Impl.write( raf, spec )

      def reader( spec: AudioFileSpec ) : ReaderFactory = {
         val spec1 = if( spec.fileType == Raw ) spec else spec.copy( fileType = Raw )
         Readable( spec1 )
      }

      private final case class Readable( spec: AudioFileSpec ) extends ReaderFactory {
         def openRead( f: File ) = sys.error("TODO"): AudioFile // XXX
         def openRead( is: InputStream ) = sys.error("TODO"): AudioFile // XXX
      }
   }

   /**
    * Sony Wave 64, the 64-bit extension of the Wave format.
    */
   case object Wave64 extends AudioFileType /* extends CanIdentify with CanRead with CanWrite XXX */ {
      final val id         = "w64"
      final val extension  = "w64"
      final val extensions = IIdxSeq( "w64", "wave64" )
   }
}
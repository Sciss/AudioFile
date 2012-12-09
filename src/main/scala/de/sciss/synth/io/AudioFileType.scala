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

import impl._
import collection.immutable.{ Set => ISet }

/**
 * A recognized audio file type.
 *
 * @param id   the unique identifier of the file type
 * @param ext  the common extension of the file type (period not included)
 */
abstract /* sealed */ class AudioFileType( val id: String, val ext: String ) {
   private[io] def factory: Option[ AudioFileHeaderFactory ]
}

object AudioFileType {
//   private val sync  = new AnyRef
   private var set   = Set[ AudioFileType ]( AIFF, NeXT, Wave, IRCAM, Wave64 )

   private[io] def register( fileType: AudioFileType ) {
//      sync.synchronized {
         set += fileType
//      }
   }

   /**
    *    Note: this Iterator does not include the Raw type
    *    which usually requires special handling.
    */
   def known : ISet[ AudioFileType ] = set // sync.synchronized { set }

   case object AIFF   extends AudioFileType( "aiff",  "aif" ) {
      private[io] def factory : Option[ AudioFileHeaderFactory ] = Some( AIFFHeader )
   }
   case object NeXT   extends AudioFileType( "next",  "snd" ) {
      private[io] def factory : Option[ AudioFileHeaderFactory ] = Some( NeXTHeader )
   }
   case object Wave   extends AudioFileType( "wav",   "wav" ) {
      private[io] def factory : Option[ AudioFileHeaderFactory ] = Some( WaveHeader )
   }

   /**
    * See http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/IRCAM/IRCAM.html
    */
   case object IRCAM  extends AudioFileType( "ircam", "irc ") {
      private[io] def factory : Option[ AudioFileHeaderFactory ] = Some( IRCAMHeader )
   }
   case object Raw    extends AudioFileType( "raw",   "raw" ) {
      private[io] def factory : Option[ AudioFileHeaderFactory ] = None // XXX
   }
   case object Wave64 extends AudioFileType( "w64",   "w64" ) {
      private[io] def factory : Option[ AudioFileHeaderFactory ] = None // XXX
   }
}
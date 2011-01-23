/*
 *  ScalaAudioFile.scala
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

import java.io.IOException

object ScalaAudioFile {
   val name          = "ScalaAudioFile"
   val version       = 0.14
   val copyright     = "(C)opyright 2004-2011 Hanns Holger Rutz"

   def versionString = (version + 0.001).toString.substring( 0, 4 )

   def main( args: Array[ String ]) {
      args.take( 2 ) match {
         case Array( "--test", path ) => {
            println( if( AudioFile.identify( path ).isDefined )
                  AudioFile.readSpec( path ).toString
               else ("Unknown file type : " + path)
            )
         }
         case _ => {
            printInfo
            System.exit( 1 )
         }
      }
   }

   def printInfo {
      println( "\n" + name + " v" + versionString + "\n" + copyright +
         ". All rights reserved.\n\nThis is a library which cannot be executed directly.\n" )
   }

   private[ io ] def opNotSupported = throw new IOException( "Operation not supported" )
}

/*
 *  ReaderFactory.scala
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

import java.io.{InputStream, File, IOException}

trait ReaderFactory {
  @throws(classOf[IOException])
  final def openRead(path: String): AudioFile = openRead(new File(path))

  /**
   * Opens an audio file for reading.
   *
   * @param  f  the path name of the file
   * @return a new <code>AudioFile</code> object
   *         whose header is already parsed and can
   *         be obtained through the <code>getDescr</code> method.
   *
   * @throws IOException if the file was not found, could not be reader
   *                     or has an unknown or unsupported format
   */
  @throws(classOf[IOException])
  def openRead(f: File): AudioFile

  @throws(classOf[IOException])
  def openRead(is: InputStream): AudioFile
}
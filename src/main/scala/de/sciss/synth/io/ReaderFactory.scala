/*
 *  ReaderFactory.scala
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.io

import java.io.{InputStream, File, IOException}

trait ReaderFactory {
  @throws(classOf[IOException])
  final def openRead(path: String): AudioFile = openRead(new File(path))

  /** Opens an audio file for reading.
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
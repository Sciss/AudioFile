package de.sciss.audiofile

import java.io.{File, IOException}

/** The JVM platform supports File I/O, i.e. opening an audio file using `openRead(f: File)`. */
trait ReaderFactoryPlatform {
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
  final def openRead(path: String): AudioFile = openRead(new File(path))
}

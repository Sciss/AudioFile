package de.sciss.audiofile

import java.io.IOException
import java.net.URI

import AudioFile.{createHeaderReaderAsync, finishOpenStreamReadAsync}

import scala.concurrent.{ExecutionContext, Future}

trait AudioFilePlatform {
  // ---- asynchronous ----

  /** Opens an audio file for asynchronous reading.
    *
    * @param  f  the path name of the file
    * @return a future  <code>AsyncAudioFile</code> object
    *         whose header is already parsed when the future completes, and
    *         that can be obtained through the <code>spec</code> method.
    *
    * @throws java.io.IOException if the file was not found, could not be reader
    *                     or has an unknown or unsupported format
    */
  @throws(classOf[IOException])
  def openReadAsync(uri: URI)(implicit executionContext: ExecutionContext): Future[AsyncAudioFile] = {
    val scheme  = uri.getScheme
    if (scheme != "idb") throw new IOException(s"Unsupported uri scheme $scheme")
    val path    = uri.getPath

    for {
      ch <- IndexedDBAsyncChannel.openRead(path)
      hr <- createHeaderReaderAsync(ch)
      af <- finishOpenStreamReadAsync(ch, hr, sourceString = uri.toString)
    } yield af
  }
}
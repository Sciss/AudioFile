package de.sciss.audiofile

import java.io.IOException
import java.net.URI

import de.sciss.audiofile.AudioFile.{createHeaderReaderAsync, createHeaderWriter, finishOpenStreamReadAsync, finishOpenStreamWriteAsync}

import scala.concurrent.{ExecutionContext, Future}

trait AudioFilePlatform {
  // ---- asynchronous ----

  /** Opens an audio file for asynchronous reading.
    *
    * @param  uri  the path to the file. Currently
    *              the 'idb' scheme is supported for accessing the browser's IndexedDB.
    * @return a future  <code>AsyncAudioFile</code> object
    *         whose header is already parsed when the future completes, and
    *         that can be obtained through the <code>spec</code> method.
    *         The future fails with `IOException` if the file was not found, could not be reader
    *         or has an unknown or unsupported format
    *
    * @throws IOException if the URI scheme is unsupported
    */
  @throws(classOf[IOException])
  def openReadAsync(uri: URI)(implicit executionContext: ExecutionContext): Future[AsyncAudioFile] = {
    val scheme  = uri.getScheme
    if (scheme != "idb") throw new IOException(s"Unsupported uri scheme $scheme")
    val path    = uri.getPath

    for {
      ch <- IndexedDBFile.openRead(path)
      hr <- createHeaderReaderAsync(ch)
      af <- finishOpenStreamReadAsync(ch, hr, sourceString = uri.toString)
    } yield af
  }

  /** Opens an audio file for asynchronous writing.
    *
    * @param  uri   the path to the file. Currently
    *               the 'idb' scheme is supported for accessing the browser's IndexedDB.
    * @param  spec  format and resolution of the new audio file.
    *               the header is immediately written to the hard-disc
    * @return       a future  <code>AsyncAudioFile</code> object for writing.
    *               The future fails with `IOException` if the file could not be opened, or there
    *               is no writer for the given audio format.

    * @throws IOException if the URI scheme is unsupported
    */
  @throws(classOf[IOException])
  def openWriteAsync(uri: URI, spec: AudioFileSpec)
                    (implicit executionContext: ExecutionContext): Future[AsyncAudioFile] = {
    val scheme  = uri.getScheme
    if (scheme != "idb") throw new IOException(s"Unsupported uri scheme $scheme")
    val path    = uri.getPath

    IndexedDBFile.openWrite(path).flatMap { ch =>
      val hw = createHeaderWriter(spec)
      finishOpenStreamWriteAsync(ch, hw, spec, sourceString = uri.toString)
    }
  }
}
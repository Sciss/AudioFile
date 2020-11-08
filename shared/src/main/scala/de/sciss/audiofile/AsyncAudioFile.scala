/*
 *  AsyncAudioFile.scala
 *  (AudioFile)
 *
 *  Copyright (c) 2004-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.audiofile

import de.sciss.asyncfile.AsyncChannel
import de.sciss.audiofile.AudioFile.Frames

import scala.concurrent.Future

trait AsyncAudioFile extends AudioFileBase with AsyncChannel {
  /** Reads sample frames from the current position
    *
    * @param  data	buffer to hold the frames reader from hard-disc.
    *               the samples will be de-interleaved such that
    *               data[0][] holds the first channel, data[1][]
    *               holds the second channel etc.
    *               ; it is allowed to have null arrays in the data
    *               (e.g. data[0] == null), in which case these channels
    *               are skipped when reading
    * @param  off  off in the buffer in sample frames, such
    *              that he first frame of the first channel will
    *              be placed in data[0][off] etc.
    * @param  len  number of continuous frames to reader.
    *
    * @return either a success, or a failure with java.io.IOException if a read error or end-of-file occurred.
    */
  def read(data: Frames, off: Int, len: Int): Future[Unit]

  final def read(data: Frames): Future[Unit] = {
    var ch  = 0
    var num = 0
    while (ch < data.length) {
      val cd = data(ch)
      if (cd != null) {
        num = cd.length
        ch  = data.length
      } else {
        ch += 1
      }
    }
    read(data, 0, num)
  }

  /** Writes sample frames to the file starting at the current position.
    *
    * @param  data	buffer holding the frames to writer to hard-disc.
    *               the samples must be de-interleaved such that
    *               data[0][] holds the first channel, data[1][]
    *               holds the second channel etc.
    * @param  off  off in the buffer in sample frames, such
    *              that he first frame of the first channel will
    *              be reader from data[0][off] etc.
    * @param  len  number of continuous frames to writer.
    *
    * @return either a success, or a failure with java.io.IOException if a write error occurred.
    */
  def write(data: Frames, off: Int, len: Int): Future[Unit]

  final def write(data: Frames): Future[Unit] = {
    var ch  = 0
    var num = 0
    while (ch < data.length) {
      val cd = data(ch)
      if (cd != null) {
        num = cd.length
        ch  = data.length
      } else {
        ch += 1
      }
    }
    write(data, 0, num)
  }

  /** Flushes pending buffer content, and
    * updates the sound file header information
    * (i.e. numFrames fields).
    */
  def flush(): Future[Unit]
}
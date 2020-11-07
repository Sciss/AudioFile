/*
 *  AudioFileBase.scala
 *  (AudioFile)
 *
 *  Copyright (c) 2004-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.audiofile

import java.io.IOException

import de.sciss.audiofile.AudioFile.Frames

trait AudioFileBase /*extends Channel*/ {
  /** Returns a description of the audio file's specification. */
  def spec: AudioFileSpec

  /** Whether it is possible to call the `read` method.
    * In general, all files opened via `openRead` or `openWrite`
    * will be readable, but external codecs may in theory be
    * writable only.
    */
  def isReadable: Boolean

  /** Whether it is possible to call the `write` method.
    * This is the case if the file was opened via `openWrite`.
    */
  def isWritable: Boolean

  /** Whether it is possible to call the `seek` method.
    * This is the case if the file was opened directly with
    * a file-based method instead of `InputStream` or `OutputStream` only.
    */
  def canSeek: Boolean

  def buffer(bufFrames: Int = 8192): Frames =
    AudioFile.buffer(numChannels, bufFrames)

  /** Returns the number of frames
    * in the file.
    *
    * @return	the number of sample frames
    *          in the file. includes pending
    *          buffer content
    *
    * @throws	java.io.IOException	this is never thrown
    *                       but declared as of the <code>InterleavedStreamFile</code>
    *                       interface
    */
  def numFrames: Long

  /** Convenience method: Returns the number of channels
    * in the file. Same as <code>spec.numChannels</code>.
    *
    * @return	the number of channels
    */
  final def numChannels: Int            = spec.numChannels

  final def sampleRate : Double         = spec.sampleRate

  final def sampleFormat: SampleFormat  = spec.sampleFormat

  final def fileType    : AudioFileType = spec.fileType

  /** Moves the file pointer to a specific
    * frame.
    *
    * @param  frame the sample frame which should be
    *               the new file position. this is really
    *               the sample index and not the physical file pointer.
    * @throws java.io.IOException when a seek error occurs or you try to
    *                     seek past the file's end.
    */
  @throws(classOf[IOException])
  def seek(frame: Long): this.type

  /** Returns the current file pointer in sample frames
    *
    * @return		the sample frame index which is the off
    *            for the next reader or writer operation.
    *
    * @throws java.io.IOException		when the position cannot be queried
    */
  def position: Long

  @throws(classOf[IOException])
  final def position_=(frame: Long): Unit = {
    seek(frame)
    ()
  }

  /** Whether or not this file is still open. */
  def isOpen: Boolean

//  /** Flushes and closes the file
//    *
//    * @throws java.io.IOException if an error occurs during buffer flush
//    *                     or closing the file.
//    */
//  @throws(classOf[IOException])
//  def close(): Unit

  /** Flushes and closes the file. As opposed
    * to <code>close()</code>, this does not
    * throw any exceptions but simply ignores any errors.
    *
    * @see	#close()
    */
  def cleanUp(): Unit
}

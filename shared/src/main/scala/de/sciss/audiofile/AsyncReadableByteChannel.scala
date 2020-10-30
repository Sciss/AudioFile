/*
 *  AsyncReadableByteChannel.scala
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

import java.nio.ByteBuffer

import scala.concurrent.{ExecutionContext, Future}

/** Similar to a mix between `java.nio.channels.AsynchronousFileChannel` and
  * `java.nio.channels.AsynchronousByteChannel`, allowing random access positioning,
  * but internally keeping track to the current position. One should assume that
  * at most one read operation can be submitted at a time (although this constraint
  * might be lifted in the future).
  */
trait AsyncReadableByteChannel extends AsyncChannel {
  var position: Long

  def size: Long

  def remaining: Long

  /** Advances the position by `len` bytes. Note that negative
    * numbers are allowed, essentially moving the position backwards.
    */
  def skip(len: Long): Unit

  implicit def executionContext: ExecutionContext

  def read(dst: ByteBuffer): Future[Int]

  /** Closes the channel. All ongoing asynchronous I/O operations become invalid. */
  def close(): Unit
}

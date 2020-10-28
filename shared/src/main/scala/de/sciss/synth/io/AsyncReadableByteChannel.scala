package de.sciss.synth.io

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.AsynchronousChannel

import scala.concurrent.{ExecutionContext, Future}

trait AsyncReadableByteChannel extends AsynchronousChannel {
  var position: Long

  def size: Long

  /** Advances the position by `len` bytes. Note that negative
    * numbers are allowed, essentially moving the position backwards.
    */
  def skip(len: Long): Unit

  implicit def executionContext: ExecutionContext

  @throws[IOException]
  def read(dst: ByteBuffer): Future[Int]
}

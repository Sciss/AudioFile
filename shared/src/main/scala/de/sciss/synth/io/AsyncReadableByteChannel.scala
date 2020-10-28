package de.sciss.synth.io

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.AsynchronousChannel

import scala.concurrent.Future

trait AsyncReadableByteChannel extends AsynchronousChannel {
  @throws[IOException]
  def read(dst: ByteBuffer): Future[Int]
}

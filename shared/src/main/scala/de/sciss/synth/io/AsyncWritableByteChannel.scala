package de.sciss.synth.io

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.AsynchronousChannel

import scala.concurrent.Future

trait AsyncWritableByteChannel extends AsynchronousChannel {
  @throws[IOException]
  def write(src: ByteBuffer): Future[Int]
}

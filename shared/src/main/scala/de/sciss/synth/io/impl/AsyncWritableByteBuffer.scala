///*
// *  AsyncWritableByteBuffer.scala
// *  (AudioFile)
// *
// *  Copyright (c) 2004-2020 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU Lesser General Public License v2.1+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth.io.impl
//
//import java.io.EOFException
//import java.nio.{Buffer, BufferOverflowException, ByteBuffer}
//
//import de.sciss.synth.io.AsyncReadableByteChannel
//
//import scala.concurrent.{ExecutionContext, Future}
//import scala.math.min
//
//final class AsyncWritableByteBuffer(ch: AsyncReadableByteChannel, capacity: Int = 128) {
//  private[this] val cap     = capacity
//  private[this] var fileRem = ch.size - ch.position
//  private[this] val arr     = new Array[Byte](cap)
//
//  val bb: ByteBuffer = ByteBuffer.wrap(arr)
//  (bb: Buffer).limit(0) // initially 'empty'
//
//  implicit val executionContext: ExecutionContext = ch.executionContext
//
//  def ensure(n: Int): Future[Unit] = {
//    val lim = bb.limit()
//    val pos = bb.position()
//    val rem = lim - pos
//    if (rem >= n) Future.successful(())
//    else {
//      val stop = rem + n
//      if (stop > cap) throw new BufferOverflowException()
//      // move remaining content to the beginning
//      System.arraycopy(arr, pos, arr, 0, rem)
//      val capM = min(cap, fileRem - rem).toInt
//      (bb: Buffer).position(rem).limit(capM)
//      //        ais.position = 0
//      val futRead = ch.read(bb)
//      futRead.map { m =>
//        if (m < n) throw new EOFException()
//        (bb: Buffer).position(0)
//        fileRem -= m
//        ()
//      }
//    }
//  }
//
//  def skip(n: Int): Unit = {
//    val lim = bb.limit()
//    val pos = bb.position()
//    val rem = lim - pos
//    if (n <= rem) {
//      (bb: Buffer).position(pos + n)
//    } else {
//      (bb: Buffer).position(0).limit(0)
//      val skipCh = n - rem
//      ch.skip(skipCh)
//    }
//  }
//
//  // synchronizes channel position with
//  // current buffer position (and sets buffer limit)
//  def purge(): Unit = {
//    val lim = bb.limit()
//    val pos = bb.position()
//    val rem = lim - pos
//    if (rem > 0) {
//      (bb: Buffer).position(0).limit(0)
//      ch.skip(-rem)
//    }
//  }
//}

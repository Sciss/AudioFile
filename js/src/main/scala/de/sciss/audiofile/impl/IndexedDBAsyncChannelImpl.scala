package de.sciss.audiofile.impl

import java.nio.ByteBuffer
import java.nio.channels.ClosedChannelException

import de.sciss.audiofile.IndexedDBAsyncChannel
import IndexedDBAsyncChannel.{mkException, mkStoreName}
import org.scalajs.dom.raw.{IDBDatabase, IDBKeyRange, IDBTransaction}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.math.min
import scala.scalajs.js

private[audiofile] final class IndexedDBAsyncChannelImpl(db: IDBDatabase, path: String, blockSize: Int, val size: Long)
  extends IndexedDBAsyncChannel {

  private[this] val storeName       = mkStoreName(path)
  private[this] val storeNames      = js.Array(storeName)
  private[this] var cachedBlockIdx  = -1
  private[this] var _open           = true
  private[this] var cacheBlock: Array[Byte] = _

  var position: Long = 0L

  def remaining: Long = size - position

  private def blockIndex  (pos: Long): Int = (pos / blockSize).toInt
  private def blockOffset (pos: Long): Int = (pos % blockSize).toInt

  /** Advances the position by `len` bytes. Note that negative
    * numbers are allowed, essentially moving the position backwards.
    */
  def skip(len: Long): Unit =
    position += len

  implicit def executionContext: ExecutionContext = ExecutionContext.global

  def read(dst: ByteBuffer): Future[Int] = {
    if (!_open) throw new ClosedChannelException

    val rem = min(dst.remaining(), remaining).toInt
    if (rem == 0) return Future.successful(0)

    val posStart  = position
    val posStop   = posStart + rem
    var bIdxStart = blockIndex  (posStart )
    var bOffStart = blockOffset (posStart )
    val bIdxStop  = blockIndex  (posStop  )
    val bOffStop  = blockOffset (posStop  )

    if (bIdxStart == cachedBlockIdx &&
        bIdxStop == cachedBlockIdx || (bIdxStop == cachedBlockIdx + 1 && bOffStop == 0)) {

      dst.put(cacheBlock, bOffStart, rem)
      position += rem
      Future.successful(rem)

    } else {
      if (bIdxStart == cachedBlockIdx) {
        val chunk = blockSize - bOffStart
        dst.put(cacheBlock, bOffStart, chunk)
        position  += chunk
        bIdxStart += 1
        bOffStart  = 0
      }

      val tx        = db.transaction(storeNames, mode = IDBTransaction.READ_ONLY)
      val store     = tx.objectStore(storeName)
//      var blocksRem = bIdxStop - bIdxStart + 1
      val keyRange  = if (bIdxStart == bIdxStop) {
        IDBKeyRange.only(bIdxStart)
      } else {
        IDBKeyRange.bound(bIdxStart, bIdxStop)
      }
      val req       = store.get(keyRange)
      val pr        = Promise[Int]()
      req.onerror   = { e =>
        pr.failure(mkException(e))
      }
      req.onsuccess = { _ =>
        println(req.result)
        pr.success(rem)
      }
      pr.future
    }
  }

  def close(): Unit =
    _open = false

  def isOpen: Boolean = _open
}

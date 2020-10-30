package de.sciss.audiofile.impl

import java.io.IOException
import java.nio.{Buffer, ByteBuffer}

import de.sciss.audiofile.IndexedDBFile.{Meta, mkStoreName, reqToFuture, writeMeta}
import de.sciss.audiofile.IndexedDWritableBFile
import org.scalajs.dom.raw.{IDBDatabase, IDBKeyRange, IDBObjectStore, IDBTransaction}

import scala.concurrent.{ExecutionContext, Future}
import scala.math.min
import scala.scalajs.js
import scala.scalajs.js.typedarray.Int8Array
import scala.scalajs.js.{typedarray => jsta}

private[audiofile] final class IndexedDBFileImpl(db: IDBDatabase, path: String, blockSize: Int, size0: Long,
                                                 readOnly: Boolean)
  extends IndexedDWritableBFile {

  private[this] val storeName       = mkStoreName(path)
  private[this] val storeNames      = js.Array(storeName)
  private[this] var cachedBlockIdx  = -1
  private[this] var _open           = true
  private[this] var cacheBlock: Array[Byte] = _
  private[this] var _position       = 0L
  private[this] var _size           = size0
  private[this] val swapBuf         = new jsta.ArrayBuffer(blockSize)
  private[this] val swapTArray      = new Int8Array(swapBuf)

  def size      : Long = _size
  def position  : Long = _position
  def remaining : Long = _size - _position

  def position_=(value: Long): Unit = {
    if (value < 0L || value > _size) throw new IOException(s"Illegal seek position $value (file size is $size)")
  }

  private def blockIndex  (pos: Long): Int = (pos / blockSize).toInt
  private def blockOffset (pos: Long): Int = (pos % blockSize).toInt

  /** Advances the position by `len` bytes. Note that negative
    * numbers are allowed, essentially moving the position backwards.
    */
  def skip(len: Long): Unit =
    position += len

  implicit def executionContext: ExecutionContext = ExecutionContext.global

  def read(dst: ByteBuffer): Future[Int] = {
    if (!_open) throw new IOException(s"File $path was already closed")

    val rem = min(dst.remaining(), remaining).toInt
    if (rem == 0) return Future.successful(rem)

    val posStart  = _position
    val posStop   = posStart + rem
    var bIdxStart = blockIndex  (posStart )
    var bOffStart = blockOffset (posStart )
    val bIdxStop  = blockIndex  (posStop  )
    val bOffStop  = blockOffset (posStop  )

    if (bIdxStart == cachedBlockIdx &&
       (bIdxStop == cachedBlockIdx || (bIdxStop == cachedBlockIdx + 1 && bOffStop == 0))) {

      dst.put(cacheBlock, bOffStart, rem)
      _position += rem
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
      val keyRange  = if (bIdxStart == bIdxStop) {
        IDBKeyRange.only(bIdxStart)
      } else {
        IDBKeyRange.bound(bIdxStart, bIdxStop)
      }
      val req       = store.get(keyRange)
      val fut       = reqToFuture(req) { _ =>
        println(s"GET returned: ${req.result}")
        // TODO: copy data; update `cacheBlock`
        rem
      }
      fut
    }
  }

  def write(src: ByteBuffer): Future[Int] = {
    if (!_open)   throw new IOException(s"File $path was already closed")
    if (readOnly) throw new IOException(s"File $path was opened for reading only")

    val writeLen = src.remaining()
    if (writeLen == 0) return Future.successful(writeLen)

    // prepare
    val posStart  = _position
    val posStop   = posStart + writeLen
    val bIdxStart = blockIndex  (posStart )
    val bOffStart = blockOffset (posStart )
    val bIdxStop  = blockIndex  (posStop  )
    val bOffStop  = blockOffset (posStop  )
    var bIdx      = bIdxStart
    var srcPos    = src.position()
    val firstLen  = if (bOffStart == 0) 0 else if (bIdxStop > bIdxStart) blockSize else bOffStop
    val lastLen   = if (bIdxStop > bIdxStart) bOffStop else 0

    val tx        = db.transaction(storeNames, mode = IDBTransaction.READ_WRITE)
    implicit val store: IDBObjectStore = tx.objectStore(storeName)

    var txn       = List.empty[Future[Unit]]

    def nextSlice(n: Int, copy: Boolean): Int8Array = {
      import jsta.TypedArrayBufferOps._
      if (src.hasTypedArray()) { // most efficient
        val srcBack = src.typedArray()
        srcBack.subarray(srcPos, srcPos + n)
      } else {
        var i = 0
        var j = srcPos
        val _swap = swapTArray
        while (i < n) {
          _swap(i) = src.get(j) // XXX TODO is there no better way?
          i += 1; j += 1
        }
        val swapSub = if (n == blockSize) _swap else _swap.subarray(0, n)
        if (copy) {
          val bufNew = new jsta.ArrayBuffer(n)
          val arrNew = new Int8Array(bufNew)
          arrNew.set(swapSub)
          arrNew
        } else {
          swapSub
        }
      }
    }

    // XXX TODO: the reqRead should set the cacheBlock

    def updateSlice(idx: Int, start: Int, stop: Int): Future[Unit] = {
      val reqRead = store.get(idx)
      val futArr  = reqToFuture(reqRead) { _ =>
        val arrOld    = reqRead.result.asInstanceOf[Int8Array]
        val arrNew    = if (arrOld.length >= stop) arrOld else {
          val bufNew  = new jsta.ArrayBuffer(stop)
          val a       = new Int8Array(bufNew)
          a.set(arrOld, 0)
          a
        }
        val arrSrc = nextSlice(stop - start, copy = false)
        arrNew.set(arrSrc, start)
        arrNew
      }
      futArr.flatMap { arrNew =>
        val reqWrite = store.put(key = idx, value = arrNew)
        reqToFuture(reqWrite)(_ => ())
      }
    }

    // first block needs to update existing data
    if (firstLen > 0) {
      // be definition (non-block aligned offset), there must be an old block
      val futFirst = updateSlice(idx = bIdxStart, start = bOffStart, stop = firstLen)
      txn        ::= futFirst
      bIdx        += 1
      srcPos      += firstLen
    }

    // "middle" blocks are put directly
    while (bIdx < bIdxStop) {
      val arrNew    = nextSlice(blockSize, copy = true)
      val reqWrite  = store.put(key = bIdx, value = arrNew)
      val futMid    = reqToFuture(reqWrite)(_ => ())
      // according to spec, it is allowed to send multiple write requests at once
      txn          ::= futMid
      srcPos        += blockSize
      bIdx          += 1
    }

    // last block
    if (lastLen > 0) {
      val hasOld  = _size > posStop - lastLen
      val futLast = if (hasOld) {
        updateSlice(idx = bIdxStop, start = 0, stop = lastLen)
      } else {
        val arrNew    = nextSlice(lastLen, copy = true)
        val reqWrite  = store.put(key = bIdxStop, value = arrNew)
        reqToFuture(reqWrite)(_ => ())
      }
      txn       ::= futLast
      srcPos     += lastLen
      bIdx       += 1
    }

    // wrap up
    val allUpdates  = Future.sequence(txn)
    val newSize     = if (posStop > _size) posStop else _size

    val futCommit = allUpdates.flatMap { _ =>
      val now = System.currentTimeMillis()
      writeMeta(Meta(blockSize = blockSize, length = newSize, lastModified = now))
    }

    futCommit.map { _ =>
      assert (src.position() + writeLen == srcPos)
      (src: Buffer).position(srcPos)
      _position = posStop
      _size     = newSize
      writeLen
    }
  }

  def close(): Unit =
    _open = false

  def isOpen: Boolean = _open
}

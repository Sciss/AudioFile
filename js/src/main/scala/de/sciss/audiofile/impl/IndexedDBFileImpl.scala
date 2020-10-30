package de.sciss.audiofile.impl

import java.io.IOException
import java.nio.{Buffer, ByteBuffer}

import de.sciss.audiofile.IndexedDBFile.{Meta, READ_ONLY, READ_WRITE, STORES_FILES, STORE_FILES, log, reqToFuture, writeMeta}
import de.sciss.audiofile.IndexedDWritableBFile
import org.scalajs.dom.raw.{IDBDatabase, IDBKeyRange, IDBObjectStore}

import scala.concurrent.{ExecutionContext, Future}
import scala.math.min
import scala.scalajs.js
import scala.scalajs.js.typedarray.Int8Array
import scala.scalajs.js.{typedarray => jsta}

private[audiofile] final class IndexedDBFileImpl(db: IDBDatabase, path: String, blockSize: Int, size0: Long,
                                                 readOnly: Boolean)
  extends IndexedDWritableBFile {

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

  private def key(idx: Int): js.Any =
    js.Array[Any](path, idx)

  private def keyRange(from: Int, until: Int): IDBKeyRange =
    IDBKeyRange.bound(js.Array(path, from), js.Array(path, until))

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

      val tx        = db.transaction(STORES_FILES, mode = READ_ONLY)
      val store     = tx.objectStore(STORE_FILES)
      val kr        = if (bIdxStart == bIdxStop) {
        key(bIdxStart)
      } else {
        keyRange(bIdxStart, bIdxStop)
      }
      val req       = store.get(kr)
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
    val firstStop = if (bOffStart == 0) 0 else if (bIdxStop > bIdxStart) blockSize else bOffStop
    val lastStop  = if (firstStop == 0 || bIdxStop > bIdxStart) bOffStop else 0

    log(s"write([pos ${src.position()}, rem ${src.remaining()}])")
    log(s"  posStart $posStart; posStop $posStop; bIdxStart $bIdxStart; bOffStart $bOffStart; bIdxStop $bIdxStop bOffStop $bOffStop")

    val tx        = db.transaction(STORES_FILES, mode = READ_WRITE)
    implicit val store: IDBObjectStore = tx.objectStore(STORE_FILES)

    var txn       = List.empty[Future[Unit]]

    // N.B. do not use outer variables here as this runs in
    // a future callback
    def nextSlice(pos: Int, n: Int, copy: Boolean): Int8Array = {
      log(s"nextSlice($n); pos = $pos")
      import jsta.TypedArrayBufferOps._
      if (src.hasTypedArray()) { // most efficient
        val srcBack = src.typedArray()
        srcBack.subarray(pos, pos + n)
      } else {
        var i = 0
        var j = pos
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

    def updateSlice(idx: Int, pos: Int, start: Int, stop: Int): Future[Unit] = {
      log(s"updateSlice($idx, $start, $stop)")
      val reqRead = store.get(key(idx))
      val futArr  = reqToFuture(reqRead) { _ =>
//        val arrOld    = reqRead.result.asInstanceOf[Int8Array]
        val arrOld    = {
          val b = reqRead.result.asInstanceOf[jsta.ArrayBuffer]
          new Int8Array(b)
        }
        val arrNew    = if (arrOld.length >= stop) arrOld else {
          val bufNew  = new jsta.ArrayBuffer(stop)
          val a       = new Int8Array(bufNew)
          a.set(arrOld, 0)
          a
        }
        val arrSrc = nextSlice(pos = pos, n = stop - start, copy = false)
        arrNew.set(arrSrc, start)
        arrNew
      }
      futArr.flatMap { arrNew =>
        val reqWrite = store.put(key = key(idx), value = arrNew.buffer)
        reqToFuture(reqWrite)(_ => ())
      }
    }

    var bIdx      = bIdxStart
    var srcPos    = src.position()

    // first block needs to update existing data
    if (firstStop > 0) {
      // be definition (non-block aligned offset), there must be an old block
      val futFirst = updateSlice(idx = bIdxStart, pos = srcPos, start = bOffStart, stop = firstStop)
      txn        ::= futFirst
      bIdx        += 1
      srcPos      += firstStop - bOffStart
    }

    // "middle" blocks are put directly
    while (bIdx < bIdxStop) {
      val arrNew    = nextSlice(pos = srcPos, n = blockSize, copy = true)
      val reqWrite  = store.put(key = key(bIdx), value = arrNew.buffer)
      val futMid    = reqToFuture(reqWrite)(_ => ())
      // according to spec, it is allowed to send multiple write requests at once
      txn          ::= futMid
      srcPos        += blockSize
      bIdx          += 1
    }

    // last block
    if (lastStop > 0) {
      val hasOld  = _size > posStop - lastStop
      val futLast = if (hasOld) {
        updateSlice(idx = bIdxStop, pos = srcPos, start = 0, stop = lastStop)
      } else {
        val arrNew    = nextSlice(srcPos, lastStop, copy = true)
        val reqWrite  = store.put(key = key(bIdxStop), value = arrNew.buffer)
        reqToFuture(reqWrite)(_ => ())
      }
      txn       ::= futLast
      srcPos     += lastStop
      bIdx       += 1
    }

    // wrap up
    val allUpdates  = Future.sequence(txn)
    val newSize     = if (posStop > _size) posStop else _size

    val futCommit = allUpdates.flatMap { _ =>
      val now = System.currentTimeMillis()
      writeMeta(path, Meta(blockSize = blockSize, length = newSize, lastModified = now))
    }

    futCommit.map { _ =>
      assert (src.position() + writeLen == srcPos, s"${src.position()} + $writeLen != $srcPos")
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

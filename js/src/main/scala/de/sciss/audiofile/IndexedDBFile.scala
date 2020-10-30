/*
 *  IndexedDBFile.scala
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

import de.sciss.audiofile.impl.IndexedDBFileImpl
import org.scalajs.dom
import org.scalajs.dom.raw.{IDBDatabase, IDBObjectStore, IDBRequest}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.{typedarray => jsta}

object IndexedDBFile {
  private val BLOCK_SIZE  = 8192
  private val VERSION     = 1
  private val META_COOKIE = 0x4d455441  // "META"
  private val KEY_META    = "meta"

  var showLog = true

  private[audiofile] def log(what: => String): Unit =
    if (showLog) println(s"[AudioFile IDB] [${new js.Date}] $what")

  object Meta {
    def fromArrayBuffer(b: js.typedarray.ArrayBuffer): Meta = {
      val bi  = new jsta.Int32Array(b)
      val cookie: Int = bi(0)
      if (cookie != META_COOKIE) {
        throw new IOException(s"Expected cookie 0x${META_COOKIE.toHexString}, but found 0x${cookie.toHexString}")
      }
      val blockSize: Int = bi(1)
      val lastModHi: Int = bi(2)
      val lastModLo: Int = bi(3)
      val lenHi    : Int = bi(2)
      val lenLo    : Int = bi(3)
      val lastModified = (lastModHi .toLong << 32) | (lastModLo .toLong & 0xFFFFFFFFL)
      val length       = (lenHi     .toLong << 32) | (lenLo     .toLong & 0xFFFFFFFFL)
      Meta(blockSize = blockSize, length = length, lastModified = lastModified)
    }
  }
  case class Meta(blockSize: Int, length: Long, lastModified: Long) {
    def toArrayBuffer: jsta.ArrayBuffer = {
      val b   = new jsta.ArrayBuffer(24)
      val bi  = new jsta.Int32Array(b)
      bi(0)   = META_COOKIE
      bi(1)   = blockSize
      bi(2)   = (lastModified >> 32).toInt
      bi(3)   =  lastModified       .toInt
      bi(4)   = (length       >> 32).toInt
      bi(5)   =  length             .toInt
      b
    }
  }

  private[audiofile] def mkExceptionMessage(e: dom.ErrorEvent): String =
    s"in ${e.filename} line ${e.lineno} column ${e.colno}: ${e.message}"

  private[audiofile] def mkException(e: dom.ErrorEvent): Exception =
    new IOException(mkExceptionMessage(e))

  private[audiofile] def mkStoreName(path: String): String = s"fs:$path"

  private[audiofile] def reqToFuture[A](req: IDBRequest, failure: dom.ErrorEvent => Throwable = mkException)
                                       (success: dom.Event => A): Future[A] = {
    val pr = Promise[A]()
    req.onerror = { e =>
      pr.failure(failure(e))
    }
    req.onsuccess = { e =>
      pr.success(success(e))
    }
    pr.future
  }

  import ExecutionContext.Implicits.global

  private def openFileSystem(): Future[IDBDatabase] = {
    val req = dom.window.indexedDB.open("fs", VERSION)

    req.onupgradeneeded = { e =>
      val db = req.result.asInstanceOf[IDBDatabase]
      val storeNames = db.objectStoreNames
      log(s"Upgrading to version ${db.version}. Deleting ${storeNames.length} local objects")
      var i = 0
      while (i < storeNames.length) {
        val storeName = storeNames(i)
        db.deleteObjectStore(storeName)
        i += 1
      }
    }

    reqToFuture(req) { _ =>
      log("Success creating/accessing IndexedDB database")
      req.result.asInstanceOf[IDBDatabase]
    }
  }

  def openRead(path: String): Future[IndexedDBFile] = {
    val dbFut = openFileSystem()
    dbFut.flatMap { db =>
      val storeName   = mkStoreName(path)
      val storeNames  = js.Array(storeName)
      val tx          = db.transaction(storeNames, mode = "readonly")
      implicit val store: IDBObjectStore = tx.objectStore(storeName)
      val futMeta     = readMeta()
      futMeta.map { meta =>
        val ch    = new IndexedDBFileImpl(
          db        = db,
          path      = path,
          blockSize = meta.blockSize,
          size0     = meta.length,
          readOnly  = true,
        )
        ch
      }
    }
  }

  def readMeta()(implicit store: IDBObjectStore): Future[Meta] = {
    val req = store.get(KEY_META)
    reqToFuture(req, e => mkException(e)) { _ =>
      val bMeta = req.result.asInstanceOf[jsta.ArrayBuffer]
      val meta  = Meta.fromArrayBuffer(bMeta)
      meta
    }
  }

  def updateMeta()(meta: Meta => Meta)(implicit store: IDBObjectStore): Future[Unit] =
    readMeta().flatMap { metaIn =>
      val metaOut = meta(metaIn)
      writeMeta(metaOut)
    }

  def writeMeta(meta: Meta)(implicit store: IDBObjectStore): Future[Unit] = {
    val bMeta = meta.toArrayBuffer
    val req   = store.put(key = KEY_META, value = bMeta)
    reqToFuture(req)(_ => ())
  }

  def openWrite(path: String, append: Boolean = false): Future[IndexedDWritableBFile] = {
    import ExecutionContext.Implicits.global
    val dbFut = openFileSystem()
    dbFut.flatMap { db =>
      val storeName   = mkStoreName(path)
      val storeNames  = js.Array(storeName)
      db.createObjectStore(storeName)
      val tx          = db.transaction(storeNames, mode = "readwrite")
      println(s"tx: $tx")
      implicit val store: IDBObjectStore = tx.objectStore(storeName)
      log(s"opened object store for $path")

      def clear(): Future[Meta] = {
        val reqClear = store.clear()
        reqToFuture(reqClear) { _ =>
          log("creating new initial meta data")
          Meta(blockSize = BLOCK_SIZE, length = 0L, lastModified = System.currentTimeMillis())
        }
      }

      val futMeta: Future[Meta] = if (append) {
        readMeta().recoverWith { case _ => clear() }
      } else {
        clear()
      }

      futMeta.map { meta =>
        new IndexedDBFileImpl(
          db        = db,
          path      = path,
          blockSize = meta.blockSize,
          size0     = meta.length,
          readOnly  = false,
        )
      }
    }
  }
}
trait IndexedDBFile extends AsyncReadableByteChannel
trait IndexedDWritableBFile extends IndexedDBFile with AsyncWritableByteChannel

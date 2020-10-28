package de.sciss.synth.io

import java.io.{FileNotFoundException, IOException}

import de.sciss.synth.io.impl.IndexedDBAsyncChannelImpl
import org.scalajs.dom
import org.scalajs.dom.raw.{IDBDatabase, IDBTransaction}

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.{typedarray => jsta}

object IndexedDBAsyncChannel {
  private val BLOCK_SIZE  = 8192
  private val VERSION     = 1
  private val META_COOKIE = 0x4d455441  // "META"
  private val KEY_META    = "meta"

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

  private[io] def mkException(e: dom.ErrorEvent): Exception = {
    val loc = s"in ${e.filename} line ${e.lineno} column ${e.colno}: ${e.message}"
    new IOException(loc)
  }

  private[io] def mkStoreName(path: String): String = s"fs:$path"

  def openRead(path: String): Future[IndexedDBAsyncChannel] = {
    val reqOpen = dom.window.indexedDB.open("fs", VERSION)
    val pr  = Promise[IndexedDBAsyncChannel]()

    reqOpen.onupgradeneeded = { e =>
      val db = reqOpen.result.asInstanceOf[IDBDatabase]
      val storeNames = db.objectStoreNames
      println(s"Upgrading to version ${db.version}. Deleting ${storeNames.length} local objects")
      var i = 0
      while (i < storeNames.length) {
        val storeName = storeNames(i)
        db.deleteObjectStore(storeName)
        i += 1
      }
    }

    reqOpen.onerror = { e =>
      println("IndexedDB database could not be created:")
      println(reqOpen.error)
      pr.failure(mkException(e))
    }

    reqOpen.onsuccess = { _ =>
      println("Success creating/accessing IndexedDB database")
      val db = reqOpen.result.asInstanceOf[IDBDatabase]
      // println("db: " + db)
      val storeName   = mkStoreName(path)
      val storeNames  = js.Array(storeName)
      val tx          = db.transaction(storeNames, mode = IDBTransaction.READ_ONLY)
      val store       = tx.objectStore(storeName)
      val reqMeta     = store.get(KEY_META)

      reqMeta.onerror = { e =>
        pr.failure(new FileNotFoundException(s"${e.message} - $path"))
      }

      reqMeta.onsuccess = { _ =>
        val bMeta = reqMeta.result.asInstanceOf[jsta.ArrayBuffer]
        val meta  = Meta.fromArrayBuffer(bMeta)
        val ch    = new IndexedDBAsyncChannelImpl(
          db        = db,
          path      = path,
          blockSize = meta.blockSize,
          size      = meta.length,
        )
        pr.success(ch)
      }
    }

    pr.future
  }
}
trait IndexedDBAsyncChannel extends AsyncReadableByteChannel {

}

package de.sciss.audiofile

import java.io.File

import de.sciss.audiofile.AudioFile.Frames
import org.scalatest.FutureOutcome
import org.scalatest.flatspec.FixtureAsyncFlatSpec

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.Future

/*
  To run only this test:

  testOnly de.sciss.audiofile.AsyncReadWriteSpec

 */
class AsyncReadWriteSpec extends FixtureAsyncFlatSpec {
  final type FixtureParam = File

//  val rwTypes: Vec[AudioFileType] = AudioFileType.readable.collect {
//    case cw: AudioFileType.CanWrite => cw
//  }

  val rwTypes: Vec[AudioFileType] = Vector(AudioFileType.AIFF)

  val chanNums: List[Int] = List(1, 2, 3)

  val bufSize    = 8192
  val totalSize  = 10000
  val size2: Int = totalSize - bufSize
  val sr         = 44100.0

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f = File.createTempFile("tmp", ".bin")
    test(f).onCompletedThen { _ =>
      if (!f.delete()) f.deleteOnExit()
    }
  }

  def generate(buf: Frames, num: Int, seed: Long = 0L): Unit = {
    val rnd = new util.Random(seed)
    var ch = 0
    while (ch < buf.length) {
      val cb = buf(ch)
      var i = 0
      while (i < num) {
        cb(i) = rnd.nextFloat()
        i += 1
      }
      ch += 1
    }
  }

  def compare(a: Frames, b: Frames, num: Int, fmt: SampleFormat): Unit = {
    val tol = fmt match {
      case SampleFormat.Float    => 0.0
      case SampleFormat.Double   => 0.0
      case _ => 2.02 / (1L << fmt.bitsPerSample) // whatever... ?
    }
    assert(a.length === b.length)
    var ch = 0
    while (ch < a.length) {
      val ca = a(ch)
      val cb = b(ch)
      var i = 0
      while (i < num) {
        val diff = math.abs(ca(i) - cb(i))
        assert(diff <= tol, "Buffer content differs (err = " + diff + ") for " + fmt)
        i += 1
      }
      ch += 1
    }
  }

  rwTypes.foreach { tpe =>
    tpe.supportedFormats.foreach { smpFmt =>
      chanNums.foreach { numCh =>
        val fileSpec = AudioFileSpec(tpe, smpFmt, numCh, sr)
        "AsyncAudioFile" should s"write and read $fileSpec" in { f =>
          val uri = f.toURI
          for {
            afOut <- AudioFile.openWriteAsync(uri, fileSpec)
            _ <- {
              assert(afOut.isOpen)
              assert(afOut.isReadable)
              assert(afOut.isWritable)
              assert(afOut.position === 0L)
              Future.unit
            }
            bufOut = afOut.buffer(bufSize)
            _ <- {
              generate(bufOut, bufSize, 0L)
              afOut.write(bufOut)
            }
            _ <- {
              generate(bufOut, size2, 1L)
              afOut.write(bufOut, 0, size2)
            }
            framesWritten <- Future(afOut.numFrames)
            _ <- {
              // println(s"SPEC ${afOut.spec}")
              assert(afOut.position === totalSize.toLong)
              assert(framesWritten === afOut.spec.numFrames)
              afOut.close()
            }
            _ <- {
              assert(!afOut.isOpen)
              assert (f.isFile)
              Future.unit
            }
            afIn <- AudioFile.openReadAsync(uri)
            bufIn = afIn.buffer(bufSize)
            _ <- {
              assert(afIn.isOpen)
              assert(afIn.isReadable)
              assert(!afIn.isWritable)
              assert(afIn.position        === 0L)
              assert(afIn.numFrames       === framesWritten)
              assert(afIn.spec.numFrames  === framesWritten)
              assert(afIn.numChannels     === numCh)
              assert(afIn.sampleFormat    === smpFmt)
              assert(afIn.sampleRate      === sr)
              assert(afIn.uri.contains(uri))
              assert(afIn.fileType        === tpe)
              afIn.read(bufIn)
            }
            _ <- {
              generate(bufOut, bufSize, 0L)
              compare(bufIn, bufOut, bufSize, smpFmt)
              afIn.read(bufIn, 0, size2)
            }
            _ <- {
              generate(bufOut, size2, 1L)
              compare(bufIn, bufOut, size2, smpFmt)
              assert(afIn.position === totalSize.toLong)
              afIn.close()
            }
            _ <- {
              assert(!afIn.isOpen)
              Future.unit
            }
          } yield assert(true)
        }
      }
    }
  }
}

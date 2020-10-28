package de.sciss.synth.io

import java.io.File
import java.util.Locale

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.math.{log10, sqrt}

object ReadAsyncTest {
  def main(args: Array[String]): Unit = {
    val directMemory = args.headOption.contains("--direct")
    sys.props.put("de.sciss.synth.io.AudioFile.DirectMemory", directMemory.toString)
    println(s"direct-memory = $directMemory")
    Locale.setDefault(Locale.US)
    val path  = "/data/projects/Anemone/rec/anemone_akademie_graz_181128.aif" // c. 1.6 GB file
    val file  = new File(path)

    runSync (file)

    implicit val ec: ExecutionContext = ExecutionContext.global
//    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(
//      java.util.concurrent.Executors.newSingleThreadExecutor()
//    )

    val fut = Future(()).flatMap { _ =>
      runAsync(file)
    }
    Await.result(fut, Duration.Inf)

//    runSync (file)

    sys.exit()
  }

  def info(meanSqr: Double, dt: Long): Unit = {
    val rms = log10(sqrt(meanSqr)) * 20
    println(f"Took ${dt.toDouble/1000}%1.3f s. RMS is $rms%1.2f dBFS.")
  }

  def runSync(file: File): Unit = {
    println("Reading file synchronously...")
    val bsz   = 8192
    var off   = 0L
    var sqrSum = 0.0
    val t1    = System.currentTimeMillis()
    val af    = AudioFile.openRead(file): AudioFile
    val N     = af.numFrames
    val buf   = af.buffer(bsz)
    val buf0  = buf(0)
    while (off < N) {
      val len = math.min(bsz, N - off).toInt
      af.read(buf, 0, len)
      var i = 0
      while (i < len) {
        val d = buf0(i)
        sqrSum += d * d
        i += 1
      }
      off += len
    }
    af.close()
    val t2 = System.currentTimeMillis()
    val meanSqr = sqrSum / N
    info(meanSqr, t2 - t1)
  }

  def runAsync(file: File)(implicit ec: ExecutionContext): Future[Unit] = {
    println("Reading file asynchronously...")
    val t1    = System.currentTimeMillis()
    val afFut = AudioFile.openReadAsync(file.toPath): Future[AsyncAudioFile]
    afFut.flatMap { af =>
      val N     = af.numFrames
      println(s"numFrames = $N")
      val bsz   = 8192
      val buf   = af.buffer(bsz)
      val buf0  = buf(0)
      var sqrSum = 0.0
//      var off   = 0L

      def loop(off: Long): Future[Unit] =
        if (off >= N) Future.successful(())
        else {
          val len = math.min(bsz, N - off).toInt
//          println(s"off = $off, len = $len, thread = ${Thread.currentThread().hashCode().toHexString}")
          val rdFut = af.read(buf, 0, len)
          rdFut.flatMap { _ =>
//            println(s"  here: ${Thread.currentThread().hashCode().toHexString}")
            var i = 0
            while (i < len) {
              val d = buf0(i)
              sqrSum += d * d
              i += 1
            }
            loop(off + len)
          }
        }

      loop(0L).andThen { case _ =>
        af.close()
      } .map { _ =>
        val t2 = System.currentTimeMillis()
        val meanSqr = sqrSum / N
        info(meanSqr, t2 - t1)
      }
    }
  }
}

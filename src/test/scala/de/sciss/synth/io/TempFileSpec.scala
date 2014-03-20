package de.sciss.synth.io

import org.scalatest.{Matchers, fixture, Outcome}
import java.io.File

trait TempFileSpec extends fixture.FlatSpec with Matchers {
  final type FixtureParam = File

  final def withFixture(test: OneArgTest): Outcome = {
    val f = File.createTempFile("tmp", ".bin")
    try {
      test(f)
    }
    finally {
      if (!f.delete()) f.deleteOnExit()
    }
  }
}

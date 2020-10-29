package de.sciss.audiofile

import java.io.File

import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait TempFileSpec extends FixtureAnyFlatSpec with Matchers {
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

package de.sciss.synth.io

import org.scalatest.fixture
import org.scalatest.matchers.ShouldMatchers
import java.io.File

trait TempFileSpec extends fixture.FlatSpec with ShouldMatchers {
   final type FixtureParam = File

   final def withFixture( test: OneArgTest ) {
      val f = File.createTempFile( "tmp", ".bin" )
      try {
         test( f )
      }
      finally {
         if( !f.delete() ) f.deleteOnExit()
      }
   }
}
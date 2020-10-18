package de.sciss.synth.io

import java.io.File

trait AudioFilePlatform {
  /** Returns the underlying <code>File</code> if it was
    * provided to the <code>AudioFile</code> constructor.
    * For an <code>AudioFile</code> created from an <code>InputStream</code>
    * this will return <code>None</code>.
    */
  def file: Option[File]
}

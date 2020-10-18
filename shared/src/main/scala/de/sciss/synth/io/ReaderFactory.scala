/*
 *  ReaderFactory.scala
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

package de.sciss.synth.io

import java.io.{InputStream, IOException}

trait ReaderFactory extends ReaderFactoryPlatform {
  @throws(classOf[IOException])
  def openRead(is: InputStream): AudioFile
}
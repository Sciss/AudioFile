/*
 *  ReaderFactory.scala
 *  (AudioFile)
 *
 *  Copyright (c) 2004-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.audiofile

import java.io.{InputStream, IOException}

trait ReaderFactory extends ReaderFactoryPlatform {
  @throws(classOf[IOException])
  def openRead(is: InputStream): AudioFile
}
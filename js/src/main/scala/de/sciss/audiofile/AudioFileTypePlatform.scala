/*
 *  AudioFileTypePlatform.scala
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

trait AudioFileTypePlatform {
  trait CanReadPlatform
  trait CanWritePlatform
  trait AIFFPlatform
  trait NeXTPlatform
  trait WavePlatform
  trait IRCAMPlatform
  trait RawPlatform
  trait ReadablePlatform
  trait Wave64Platform
}

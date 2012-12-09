/*
 *  AudioFileSpec.scala
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	 For further information, please contact Hanns Holger Rutz at
 *	 contact@sciss.de
 */

package de.sciss.synth.io

import java.nio.ByteOrder

//trait AudioFileSpecLike {
//   def fileType: AudioFileType
//   def sampleFormat: SampleFormat
//   def numChannels: Int
//   def sampleRate: Double
//   def numFrames: Long
//}
//
//trait AudioFileInputSpec extends AudioFileSpecLike {
//   def byteOrder: ByteOrder
//   def toOutput : AudioFileSpec
//}

case class AudioFileSpec( fileType: AudioFileType = AudioFileType.AIFF, sampleFormat: SampleFormat = SampleFormat.Float,
                          numChannels: Int, sampleRate: Double, byteOrder: Option[ ByteOrder ] = None, numFrames: Long = 0L )
// extends AudioFileSpecLike
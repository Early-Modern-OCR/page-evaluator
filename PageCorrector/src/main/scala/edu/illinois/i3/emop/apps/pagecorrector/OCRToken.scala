package edu.illinois.i3.emop.apps.pagecorrector

abstract class OCRToken(val id: String, val text: String) extends Token with PunctTrimmer {
  lazy val cleanedText = trimPunct(trimmed)

  val isCorrectable: Boolean
}

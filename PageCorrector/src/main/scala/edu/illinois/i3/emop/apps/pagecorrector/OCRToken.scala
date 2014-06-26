package edu.illinois.i3.emop.apps.pagecorrector

abstract class OCRToken(val id: String, val text: String) extends Token {
  val isCorrectable: Boolean
}

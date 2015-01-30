package edu.illinois.i3.emop.apps.pagecorrector

/**
 * Base class for tokens generated from an OCR process
 *
 * @param id The token id
 * @param text The token text
 */
abstract class OCRToken(val id: String, val text: String) extends Token {

  /**
   * Flag indicating whether this token is correctable
   */
  val isCorrectable: Boolean
}

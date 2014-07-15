package edu.illinois.i3.emop.apps.pagecorrector

trait UTF8Normalizer {

  /**
   * Cleans the token by replacing UTF8 versions of the dash and quote characters with their regular counterparts
   * @param text The text to clean
   * @return The cleaned text
   */
  def normalizeUTF8(text: String) = text.replaceAll("[’‘´]", "'").replaceAll("[–—]", "-")

}

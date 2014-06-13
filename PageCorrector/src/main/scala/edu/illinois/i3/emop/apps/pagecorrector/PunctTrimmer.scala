package edu.illinois.i3.emop.apps.pagecorrector

trait PunctTrimmer {
  protected val MAX_LEADING_PUNCT_TO_REMOVE: Int
  protected val MAX_TRAILING_PUNCT_TO_REMOVE: Int

  def trimPunct(text: String) =
    text.replaceFirst(s"^\\p{Punct}{0,$MAX_LEADING_PUNCT_TO_REMOVE}", "")
      .replaceFirst(s"\\p{Punct}{0,$MAX_TRAILING_PUNCT_TO_REMOVE}$$", "")
}
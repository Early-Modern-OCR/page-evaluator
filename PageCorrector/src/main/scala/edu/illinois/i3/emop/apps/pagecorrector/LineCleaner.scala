package edu.illinois.i3.emop.apps.pagecorrector

import edu.illinois.i3.emop.apps.pagecorrector.PageParser._

trait LineCleaner {
  type TokenType <: Token

  /**
   * Cleans each line by removing empty tokens and then any empty lines
   * (this is usually a necessary step before joining hyphenated words)
   *
   * @param lines The lines
   * @return The cleaned lines
   */
  def cleanLines(lines: Seq[Line[TokenType]]) = lines.map(_.filter(_.trimmed.nonEmpty)).filter(_.nonEmpty)
}

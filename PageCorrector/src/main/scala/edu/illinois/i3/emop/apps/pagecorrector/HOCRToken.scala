package edu.illinois.i3.emop.apps.pagecorrector

import scala.collection.mutable

object HOCRToken {
  import java.util.regex.Pattern

  protected val CLEAN_TOKEN_LEN_THRESHOLD = 3
  protected val HEUR_MIN_NO_PUNCT_LEN = 2
  protected val MAX_DICT_SUGGESTION_LEVENSHTEIN = 200

  protected val NonAlphaPattern = Pattern.compile("\\P{L}", Pattern.CANON_EQ)
  protected val Repeated4orMoreCharsPattern = Pattern.compile("(\\P{N})\\1{3,}", Pattern.CANON_EQ)

  def preservePunctuationAndStyle(original: String, replacementCandidate: String) = {
    val replacementText = replacementCandidate.toLowerCase
    val beginPunct = original.takeWhile(!_.isLetter)
    val endPunct = original.reverse.takeWhile(!_.isLetter).reverse
    val cleanedToken = original.substring(beginPunct.length, original.length - endPunct.length)
    val isAllUpper = (cleanedToken.count(_.isUpper).toFloat / cleanedToken.length) > 0.7f
    val isFirstUpper = cleanedToken.head.isUpper
    val replacement = (isAllUpper, isFirstUpper) match {
      case (true, _) => replacementText.toUpperCase
      case (_, true) => s"${replacementText.head.toUpper}${replacementText.tail}"
      case _ => replacementText
    }
    beginPunct concat replacement concat endPunct
  }
}

abstract class HOCRToken(id: String, text: String) extends OCRToken(id, text) with SpellChecker with TextTransformer {
  import HOCRToken._

  protected override val MAX_LEADING_PUNCT_TO_REMOVE = 2
  protected override val MAX_TRAILING_PUNCT_TO_REMOVE = 3

  /**
   *   Determines whether the token matches the correctable profile
   *
   *   correctable profile:
   *        tokens which, after cleaning, contain at most 2 non-alpha characters and
   *        at least 1 alpha character, have a length of at least `CLEAN_TOKEN_LEN_THRESHOLD`, and do not
   *        contain 4 or more repeated characters in a run
   */
  lazy val isCorrectable = {
    import edu.illinois.i3.scala.utils.implicits.RegexImplicits._

    lazy val nonAlphaCharCount = NonAlphaPattern.matcher(cleanedText).findCount
    lazy val alphaCharCount = cleanedText.length - nonAlphaCharCount
    lazy val has4orMoreRepeatedChars = Repeated4orMoreCharsPattern.matcher(cleanedText).find()

    cleanedText.length >= CLEAN_TOKEN_LEN_THRESHOLD &&
      nonAlphaCharCount < 3 &&
      alphaCharCount > 0 &&
      !has4orMoreRepeatedChars
  }

  /**
   * Stores all candidates for replacement for the token (if misspelled) - includes the original (cleaned) token
   * to account for situations when the original token is correct but the dictionary does not know about it <br>
   * Note: The original (cleaned) token is always first in the set of replacements
   */
  lazy val replacements =
    Seq(cleanedText) ++ {
      if (isMisspelled)
        heuristicReplacements ++ correctTransformations.map(_.text) ++ dictSuggestions
      else
        Nil
    }

  /**
   * Stores the correct replacements of an original text based on all possible transformations to it
   */
  lazy val correctTransformations = transformations(cleanedText).filter(t => isCorrect(t.text))

  /**
   * Stores suggestions coming from dictionary (based on configured Levenshtein distance transforms)
   */
  lazy val dictSuggestions = getSuggestions(cleanedText, MAX_DICT_SUGGESTION_LEVENSHTEIN).map(_.getWord)

  /**
   * Stores replacement candidates generated based on heuristics
   */
  lazy val heuristicReplacements = {
    var candidates = Set.empty[String]

    // remove all punctuation and check whether resulting text is a dictionary word of length at least 3
    val noPunctText = text.collect { case c: Char if c.isLetter => c }
    if (noPunctText.length != cleanedText.length && noPunctText.length > HEUR_MIN_NO_PUNCT_LEN && isCorrect(noPunctText))
      candidates += noPunctText

    candidates
  }

  /**
   * Stores whether the token is misspelled.  If a token is not correctable,
   * then it's neither correctly spelled nor misspelled.
   */
  lazy val isMisspelled = if (isCorrectable) !isCorrect(cleanedText) else false

  protected val contextMatches = mutable.HashMap.empty[String, mutable.HashSet[TokenContextMatch]]

  def addContextMatch(position: Int, text: String, matchCount: Int, volCount: Int) {
    contextMatches.getOrElseUpdate(text, { mutable.HashSet.empty[TokenContextMatch] }) +=
      TokenContextMatch(position, matchCount, volCount)
  }

  def sortedContextMatches = contextMatches.toSeq.sortWith((a,b) => (a._2, b._2) match {
    case (m1, m2) if m1.size == m2.size =>
      val m1VolCount = m1.map(_.volCount).sum
      val m2VolCount = m2.map(_.volCount).sum
      if (m1VolCount == m2VolCount)
        m1.map(_.matchCount).sum > m2.map(_.matchCount).sum
      else
        m1VolCount > m2VolCount
    case (m1, m2) => m1.size > m2.size
  })

  def bestUnformattedReplacement = sortedContextMatches.map(_._1).headOption match {
    case m @ Some(bestMatch) => m
    case None if replacements.size == 2 => Some(replacements.tail.head) // skip the first, which is always the original token
    case _ => None
  }

  def bestReplacement = bestUnformattedReplacement match {
    case Some(bestMatch) => Some(preservePunctuationAndStyle(text, bestMatch))
    case _ => None
  }

  override def toString = text
}

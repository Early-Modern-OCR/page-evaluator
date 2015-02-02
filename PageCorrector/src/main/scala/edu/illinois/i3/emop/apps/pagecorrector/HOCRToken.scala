package edu.illinois.i3.emop.apps.pagecorrector

import edu.illinois.i3.emop.apps.pagecorrector.TextTransformer.{Transformation, TransformedText}

import scala.collection.mutable
import Regexps._

object HOCRToken {

  protected val CORRECTABLE_TOKEN_MIN_LEN = 2
  protected val CORRECTABLE_TOKEN_MAX_LEN = 18
  protected val REPLACED_CHARS_THRESHOLD = 0.6
  protected val MAX_NONTRANSFORMABLE_CHARS_ALLOWED = 2
  protected val MAX_DICT_SUGGESTION_LEVENSHTEIN = 200
  protected val BEGIN_PUNCT_ALLOWED = Set('(', '[', '{', '\'', '"')

  def preservePunctuationAndStyle(token: HOCRToken, replacementCandidate: String) = {
    val tokenText = token.text
    val replacementText = replacementCandidate.toLowerCase
    val beginPunct = BeginPunctPattern.findFirstIn(tokenText).getOrElse("")
    var endPunct = EndPunctPattern.findFirstIn(tokenText).getOrElse("")
    if (endPunct.nonEmpty) {
      // before adding back end punctuation (as requested), check whether some of it has been
      // used in transforming the word based on the transformation rules, and if so ignore that
      token.correctTransformations.find(_.text equalsIgnoreCase replacementCandidate) match {
        case Some(TransformedText(_, original, trans)) if trans.nonEmpty =>
          val origEndPunct = EndPunctPattern.findFirstIn(original).getOrElse("")
          // get last index after which everything is considered punctuation
          val lastStartPunctIdx = original.length - origEndPunct.length
          // find the index in the original text that marks the point after which no more transformations were applied
          val endTransIdx = trans.init.foldLeft(trans.last.index + trans.last.original.length)((adj, t) => adj + t.original.length - t.replacement.length)
          // isolate the punctuation after 'lastStartPunctIdx' that was used in transformations (we don't want to add this back based on the "preserve end punctuation" policy)
          val ignorePunct = if (lastStartPunctIdx < endTransIdx) original.substring(lastStartPunctIdx, endTransIdx) else ""
          if (endPunct startsWith ignorePunct)
            endPunct = endPunct.drop(ignorePunct.length)

        case _ =>
      }
    }
    val cleanedToken = tokenText.substring(beginPunct.length, tokenText.length - endPunct.length)
    val isAllUpper = (cleanedToken.count(_.isUpper).toFloat / cleanedToken.length) > 0.7f
    val isFirstUpper = cleanedToken.head.isUpper
    val replacement = (isAllUpper, isFirstUpper) match {
      case (true, _) => replacementText.toUpperCase
      case (_, true) => s"${replacementText.head.toUpper}${replacementText.tail}"
      case _ => replacementText
    }
    val keepBeginPunct = beginPunct.reverse.takeWhile(BEGIN_PUNCT_ALLOWED.contains).reverse
    keepBeginPunct concat replacement concat endPunct
  }
}

abstract class HOCRToken(id: String, text: String, val noiseConf: Float, val maxTransformCount: Int) extends OCRToken(id, text) with SpellChecker with TextTransformer {
  import HOCRToken._

  /**
   * Finds the longest contiguous substrings (start,end)-pairs of the original token text that can
   * potentially be corrected (taking into account the provided transformations)
   *
   * TODO: ideally, each of the groupings should become a token to correct
   */
   protected lazy val correctableParts = findTransformableParts(text, MAX_NONTRANSFORMABLE_CHARS_ALLOWED)

   protected lazy val bestCorrectablePart = {
    implicit val ordering = new Ordering[List[(Int,Int)]] {
      override def compare(x: List[(Int, Int)], y: List[(Int, Int)]) = {
        val s1 = text.substring(x.head._1, x.last._2)
        val s2 = text.substring(y.head._1, y.last._2)
        val s1_l = s1.count(_.isLetter)
        val s2_l = s2.count(_.isLetter)
        if (s1_l != s2_l)
          s1_l.compareTo(s2_l)
        else
          s1.length.compareTo(s2.length)
      }
    }

    val bestPart = correctableParts match {
      case Nil => List.empty[(Int,Int)]
      case _ => correctableParts.max
    }

    bestPart
  }

  lazy val correctableText = bestCorrectablePart match {
    case Nil => text
    case _ => text.substring(bestCorrectablePart.head._1, bestCorrectablePart.last._2)
  }

  /**
   *   Determines whether the token matches the correctable profile
   *
   *   correctable profile:
   *        tokens which, after cleaning, contain at least 1 alpha character,
   *        have a length of at least `CORRECTABLE_TOKEN_MIN_LEN`, and do not
   *        contain 4 or more repeated characters in a run
   */
  lazy val isCorrectable = {
    import edu.illinois.i3.scala.utils.implicits.RegexImplicits._

    lazy val alphaCharCount = AlphaPattern.matcher(correctableText).findCount
    lazy val has4orMoreRepeatedChars = Repeated4orMoreCharsPattern.matcher(correctableText).find()

    correctableParts.nonEmpty &&
      correctableText.length >= CORRECTABLE_TOKEN_MIN_LEN &&
      alphaCharCount > 0 &&
      !has4orMoreRepeatedChars
  }

  protected lazy val correctableTextWithoutPunctuation = correctableText.collect { case c if c.isLetter => c }

  /**
   * Stores whether the token is misspelled.  If a token is not correctable,
   * then it's neither correctly spelled nor misspelled.
   */
  lazy val isMisspelled = if (isCorrectable) !isCorrect(correctableText) else false

  /**
   * Use the original token and the token with all punctuation removed as potential replacements to allow the context
   * matching to pick them as "good" if they happen to be correct words that do not exist in our dictionaries (since all
   * other suggestions our algorithm makes are only words in the dictionary)
   */
  protected lazy val defaultReplacements = Seq(correctableText, correctableTextWithoutPunctuation)
  lazy val defaultReplacementsCount = defaultReplacements.size

  /**
   * Stores all candidates for replacement for the token (if misspelled) - includes the original (cleaned) token
   * to account for situations when the original token is correct but the dictionary does not know about it <br>
   * Note: The original (cleaned) token is always first in the set of replacements
   */
  lazy val replacements =
    if (isMisspelled && correctableText.length < CORRECTABLE_TOKEN_MAX_LEN) {
      val transformCorrections = correctTransformations.map(_.text)
      var combined = defaultReplacements ++ transformCorrections
      if (transformCorrections.size != 1 && correctableText.length > 3)
        // only add dictionary suggestions if the token is 4 or more characters long
        // for tokens less than 4 characters, rely only on the transformation candidates
        combined ++= dictSuggestions

      import edu.illinois.i3.scala.utils.implicits.StringsImplicits._

      // drop all candidate replacements for which all characters change compared to the original
      combined.filterNot(_.editDistance(correctableText) == correctableText.length)
    } else
      defaultReplacements

  /**
   * Stores the correct replacements of the token based on all possible transformations to its cleaned version
   */
  lazy val correctTransformations = {
    val interSpanChars = bestCorrectablePart.sliding(2).collect { case List((_,b), (c,_)) => text.substring(b, c) }.toList

    def spanToText(spans: List[(Int,Int)]) = spans.sorted.map { case (s, e) => text.substring(s, e) }.mkString

    /**
     * WARNING: A very expensive operation - avoid if possible
     *
     * Merges two sets of transformations
     *
     * @param t1 The first set of transformations
     * @param t2 The second set of transformations
     * @param sep The separator to use between them
     * @return The merged sets
     */
    def join(t1: Iterable[TransformedText], t2: Iterable[TransformedText], sep: String = ""): Iterable[TransformedText] =
      for { left <- t1; right <- t2 }
      yield TransformedText(
        text = left.text + sep + right.text,
        original = left.original + sep + right.original,
        transformations = left.transformations ++
          right.transformations.map(t => t.copy(index = t.index + left.original.length + sep.length))
      )

    val transforms = interSpanChars match {
      case spanChars if spanChars.isEmpty || spanChars.forall(_ equals "-") => transformations(correctableText, maxTransformCount)
        // FIXME: This case deals with words with apostrophes by trying to transform each side of the apos, but it's a very expensive operation
//      case _ ::> last if last == "'" && bestCorrectablePart.last._2 - bestCorrectablePart.last._1 < 4 =>
//        join(
//          transformations(spanToText(bestCorrectablePart.init), maxTransformCount),
//          transformations(spanToText(List(bestCorrectablePart.last)), maxTransformCount),
//          "'"
//        )
      case _ => transformations(spanToText(bestCorrectablePart), maxTransformCount)
    }

//    val spanTransforms = bestCorrectablePart.map(span => text.substring(span._1, span._2)).map(transformations(_))
//
//
//    def combineTransforms(t: List[Seq[TransformedText]]) =
//      t.reduceLeft((transformAcc, transform) => join(transformAcc, transform))
//
//    val transforms = interSpanChars match {
//      case Nil => combineTransforms(spanTransforms)
//      // Treat tokens ending in '<char> specially
//      case init ::> last if last equals "'" => join(combineTransforms(spanTransforms.init), spanTransforms.last, "'")
//      case _ => combineTransforms(spanTransforms)
//    }

    transforms
      .map {
        case tt @ TransformedText(txt, original, trans) =>
          // remove all punctuation other than hyphen and apostrophe from the transformed text
          // since it will not be found in the dictionary; this has the chance of fixing
          // errors such as m.0.th.e.r which should become "mother"
          val newTxt = txt.replaceAll("""[^\pL\pM\pN-']+""", "")
          val diff = txt.length - newTxt.length
          if (diff > 0)
            tt.copy(text = newTxt)
          else
            tt
          // remove punctuation at the beginning and end of a candidate transformation
          // to allow tokens such as "t()()!" to be corrected to "too" (extra punctuation not used in transformation will
          // be added back in the last step via the 'preservePunctuationAndStyle' method)
//          val beginPunctCount = BeginPunctPattern.findFirstIn(txt).getOrElse("").length
//          val endPunctCount  = EndPunctPattern.findFirstIn(txt).getOrElse("").length
//          if (beginPunctCount + endPunctCount > 0) {
//            val newTxt = txt.drop(beginPunctCount).dropRight(endPunctCount)
//            tt.copy(text = newTxt)
//          } else
//            tt
      }
      .filter(t => t.text.length >= CORRECTABLE_TOKEN_MIN_LEN && isCorrect(t.text))
  }

  /**
   * Stores suggestions coming from dictionary (based on configured Levenshtein distance transforms)
   */
  lazy val dictSuggestions = getSuggestions(correctableText, MAX_DICT_SUGGESTION_LEVENSHTEIN).collect {
    case suggestion if suggestion.getWord.length >= CORRECTABLE_TOKEN_MIN_LEN => suggestion.getWord
  }

  protected val contextMatches = mutable.HashMap.empty[String, mutable.HashSet[TokenContextMatch]]

  def addContextMatch(position: Int, matchText: String, matchCount: Int, volCount: Int) {
    contextMatches.getOrElseUpdate(matchText, { mutable.HashSet.empty[TokenContextMatch] }) +=
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
    // context matches always win
    case m @ Some(bestMatch) => m
    // if there were no context matches, but there's only one correction possible (outside of the default ones) then return that
    case None if replacements.size - defaultReplacementsCount == 1 =>
      val replacement = replacements.last
      val replacedCharsCount = correctableText.length - correctableText.intersect(replacement).length
      if (replacedCharsCount.toFloat / correctableText.length <= REPLACED_CHARS_THRESHOLD)
        Some(replacements.last)
      else
        None
    // if we end up with only the default replacements, check to see if exactly one of them is correct and use that, otherwise leave as-is
    case None if replacements.size == defaultReplacementsCount && defaultReplacements.tail.count(isCorrect) == 1 =>
      defaultReplacements.tail.find(isCorrect)
    // if we have more than one suggestion in addition to the defaults and context does not have any matches, then
    // look at the default suggestion created by removing all punctuation, and if that is a correct word, make that
    // as correction since it's likely the correct replacement
    case None if isCorrect(correctableTextWithoutPunctuation) => Some(correctableTextWithoutPunctuation)
    case _ => None
  }

  // only allow replacements with length > 1
  def bestReplacement = bestUnformattedReplacement.filter(_.length > 1).map(preservePunctuationAndStyle(this, _)).filter(_ != text)

  override def toString = text
}

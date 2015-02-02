package edu.illinois.i3.emop.apps.pagecorrector

import com.typesafe.scalalogging.LazyLogging
import org.w3c.dom.{Element,Document}

import edu.illinois.i3.spellcheck.engine.SpellDictionary

import scala.util.{Failure, Success}
import Regexps._


/**
 * Implementation of the page corrector algorithm for the eMOP project
 *
 * @param dictionaries The dictionaries to use to check the correctness of tokens extracted from page hOCR
 * @param transformRules The transformation rules to use to generate alternatives for misspelled tokens
 * @param contextChecker The context checker to use to find the appropriate correction to make given the context
 */
class EmopPageCorrector(val dictionaries: Iterable[SpellDictionary],
                        val transformRules: TextTransformer.TransformRules,
                        val contextChecker: NgramContextMatcher,
                        val noiseCutoff: Float,
                        val maxTransformCount: Int)
  extends HOCRPageParser with LineCleaner with UTF8Normalizer with HyphenatedTokenJoiner with LazyLogging {
  corrector =>

  override type TokenType = HOCRToken
  override type HyphenatedTokenType = HyphenatedToken

  /**
   * Factory method for creating hOCR tokens
   *
   * @param xmlWord The xml token
   * @return The hOCR token
   */
  protected override def createToken(xmlWord: Element): HOCRToken = {
    val id = xmlWord.getAttribute("id")
    val props = getProperties(xmlWord.getAttribute("title"))
    val noiseConf = props.getOrElse("noiseConf", "0") match {
      case "NaN" => 0f
      case value => value.toFloat
    }

    val text = normalizeUTF8(xmlWord.getTextContent)

    new HOCRToken(id, text, noiseConf, maxTransformCount) {
      override val dictionaries = corrector.dictionaries
      override val transformRules = corrector.transformRules
    }
  }

  protected override def createHyphenatedToken(firstToken: TokenType, secondToken: TokenType): HyphenatedToken =
    new HyphenatedToken(firstToken, secondToken, maxTransformCount) {
      override val dictionaries = corrector.dictionaries
      override val transformRules = corrector.transformRules
    }

  override def getLines(document: Document) =
    joinHyphenatedTokens(cleanLines(super.getLines(document).map(line => line.filter(token => token.noiseConf < noiseCutoff || noiseCutoff == 0))))

  /**
   * Checks whether this 3-gram window should be considered for context-matching.
   * For example, we want to return 'false' for 3-gram windows that do not contain any misspelled words,
   * since there's no point in doing context-checking if everything is spelled correctly in this window.
   *
   * @param window The window
   * @return True if this window should be considered for context-matching, False otherwise
   */
  protected def hasMisspelledCorrectableToken(window: Seq[HOCRToken]): Boolean =
    window.length == 3 && window.exists(token => token.isMisspelled)

  /**
   * Checks if the generated replacements for each token in the window meet the minimum requirements to be used for context matching
   *
   * @param tokenReplacements The candidate replacements
   * @return True if valid, False otherwise
   */
  protected def isValidWindowForContextMatching(tokenReplacements: ((HOCRToken, Iterable[String]), (HOCRToken, Iterable[String]), (HOCRToken, Iterable[String]))) =
    tokenReplacements match {
      case ((token1, ngram1), (token2, ngram2), (token3, ngram3)) =>
        ngram1.nonEmpty && ngram2.nonEmpty && ngram3.nonEmpty &&
          (
            if (ngram1.size + ngram2.size + ngram3.size == 3)
              ngram1.exists(_ != token1) || ngram2.exists(_ != token2) || ngram3.exists(_ != token3)
            else
              true
          )
    }

  /**
   * Checks whether the given replacement is valid for context matching purposes
   *
   * Note: The current 3-gram context matching database only contains ngrams made up exclusively of characters,
   * so there is no point in querying the database for replacements that contain punctuation or digits. This method
   * should be adjusted if the 3-gram database is changed
   *
   * TODO: ideally the regexp should come from a configuration file
   *
   * @param replacement The replacement
   * @return True if valid to use for context matching, False otherwise
   */
  protected def isValidReplacementForContextMatching(replacement: String) = AllAlphaPattern.matcher(replacement).matches()

  /**
   * Performs the correction of the given tokens based on the transformation rules, context matching, and
   * heuristic rules put in place in the algorithm
   *
   * @param tokens The tokens to correct
   */
  def correctTokens(tokens: Seq[HOCRToken]) {
    def generateCandidateReplacements(token: HOCRToken): Iterable[String] = {
      val candidates =
        if (token.isMisspelled) {
          logger.debug("Finding candidate replacements for {}", token)
          token.replacements.map(_.toLowerCase).toSet
        } else Set(token.correctableText)

      candidates.filter(isValidReplacementForContextMatching)
    }

    // use a sliding window of 3 tokens (3-grams) to find windows containing misspelled tokens that need to be corrected
    def generateWindowCandidates(window: Seq[HOCRToken]) =  window match {
      case Seq(token1, token2, token3) =>
        logger.debug("window: {} {} {}", token1, token2, token3)
        val ngram1 = generateCandidateReplacements(token1)
        val ngram2 = generateCandidateReplacements(token2)
        val ngram3 = generateCandidateReplacements(token3)

        (token1 -> ngram1, token2 -> ngram2, token3 -> ngram3)
    }

    tokens.sliding(3)
      .withFilter(hasMisspelledCorrectableToken)
      .map(generateWindowCandidates)
      .withFilter(isValidWindowForContextMatching)
      .foreach(checkContext)
  }

  /**
   * Performs context checking for the 3-grams generated by the combinations of replacements for each token
   * This method has the side effect of adding context matches to each token for which a match was found.
   *
   * @param replacements The candidate replacements
   */
  protected def checkContext(replacements: ((HOCRToken, Iterable[String]), (HOCRToken, Iterable[String]), (HOCRToken, Iterable[String]))): Unit =
    replacements match {
      case ((token1, ngram1), (token2, ngram2), (token3, ngram3)) =>
        logger.debug("Candidates used for context matching:")
        logger.debug("{} -> {}", token1.text, ngram1)
        logger.debug("{} -> {}", token2.text, ngram2)
        logger.debug("{} -> {}", token3.text, ngram3)

        // use the context checker to find the best context matches in the (sanitized) Google 3-gram dataset
        contextChecker.matches(ngram1, ngram2, ngram3) match {
          case Success(contextMatches) if contextMatches.nonEmpty => contextMatches.foreach {
            case ContextMatch(text1, text2, text3, matchCount, volCount) =>
              logger.debug("ContextMatch: {} {} {}  (matchCount: {} , volCount: {})",
                text1, text2, text3, matchCount.toString, volCount.toString)
              if (token1.isMisspelled) token1.addContextMatch(1, text1, matchCount, volCount)
              if (token2.isMisspelled) token2.addContextMatch(2, text2, matchCount, volCount)
              if (token3.isMisspelled) token3.addContextMatch(3, text3, matchCount, volCount)
          }

          case Failure(e) =>
            throw EmopCorrectorException(s"Failed context match (ngram1: $ngram1, ngram2: $ngram2, ngram3: $ngram3)", e)

          case _ => logger.debug("No 3-gram context matches found") // TODO: check for 2-gram matches?
        }
    }

  private def getProperties(s: String) = {
    val Pattern = """(\S+) (.+)""".r
    s.split(';').map(_.trim).collect { case Pattern(key, value) => (key, value) }.toMap
  }

}

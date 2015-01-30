package edu.illinois.i3.emop.apps.pagecorrector

import com.typesafe.scalalogging.LazyLogging
import org.w3c.dom.{Element,Document}

import edu.illinois.i3.spellcheck.engine.SpellDictionary

import scala.collection.mutable
import scala.util.{Failure, Success}

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
  protected def isCandidateForContextMatching(window: Seq[HOCRToken]): Boolean =
    window.exists(token => token.isMisspelled)

  /**
   * Performs the correction of the given tokens based on the transformation rules, context matching, and
   * heuristic rules put in place in the algorithm
   *
   * @param tokens The tokens to correct
   */
  def correctTokens(tokens: Seq[HOCRToken]) {
    // keep a cache of already calculated candidate replacements (not absolutely necessary to do this, but...)
    val replacementCache = mutable.HashMap.empty[String, Iterable[String]]

    // use a sliding window of 3 tokens (3-grams) to find windows containing misspelled tokens that need to be corrected
    tokens.sliding(3).withFilter(isCandidateForContextMatching).foreach {
      case window @ Seq(token1, token2, token3) =>
        logger.debug("window: {} {} {}", token1, token2, token3)

        // find candidates for the first token in the window, if misspelled
        val ngram1 = if (token1.isMisspelled) {
          logger.debug("Finding candidate replacements for {}", token1)
          replacementCache.getOrElseUpdate(token1.correctableText, {
            token1.replacements.map(_.toLowerCase).toSet
          })
        } else Set(token1.correctableText)

        // find candidates for the second token in the window, if misspelled
        val ngram2 = if (token2.isMisspelled) {
          logger.debug("Finding candidate replacements for {}", token2)
          replacementCache.getOrElseUpdate(token2.correctableText, {
            token2.replacements.map(_.toLowerCase).toSet
          })
        } else Set(token2.correctableText)

        // find candidates for the third token in the window, if misspelled
        val ngram3 = if (token3.isMisspelled) {
          logger.debug("Finding candidate replacements for {}", token3)
          replacementCache.getOrElseUpdate(token3.correctableText, {
            token3.replacements.map(_.toLowerCase).toSet
          })
        } else Set(token3.correctableText)

        // check if we found candidates (ignore if not)
        if (ngram1.size + ngram2.size + ngram3.size > 3) {
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

            case _ => logger.debug("No 3-gram context matches found")  // TODO check for 2-gram matches?
          }
        }

      case _ => // skip context matching if page has < 3 tokens
    }
  }

  private def getProperties(s: String) = {
    val Pattern = """(\S+) (.+)""".r
    s.split(';').map(_.trim).collect { case Pattern(key, value) => (key, value) }.toMap
  }

}

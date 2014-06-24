package edu.illinois.i3.emop.apps.pagecorrector

import org.w3c.dom.{Element,Document}

import com.typesafe.scalalogging.slf4j.Logging
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
                        val contextChecker: NgramContextMatcher)
  extends HOCRPageParser with LineCleaner with HyphenatedTokenJoiner with Logging {
  corrector =>

  override type TokenType = HOCRToken
  override type HyphenatedTokenType = HyphenatedToken

  protected override def createToken(xmlWord: Element) = {
    val id = xmlWord.getAttribute("id")
    val text = xmlWord.getTextContent
    new HOCRToken(id, text) {
      override val dictionaries = corrector.dictionaries
      override val transformRules = corrector.transformRules
    }
  }

  protected override def createHyphenatedToken(firstToken: TokenType, secondToken: TokenType) =
    new HyphenatedToken(firstToken, secondToken) {
      override val dictionaries = corrector.dictionaries
      override val transformRules = corrector.transformRules
    }

  override def getLines(document: Document) = joinHyphenatedTokens(cleanLines(super.getLines(document)))

  def correctTokens(tokens: Seq[HOCRToken]) {
    // keep a cache of already calculated candidate replacements (not absolutely necessary to do this, but...)
    val replacementCache = mutable.HashMap.empty[String, Iterable[String]]

    // use a sliding window of 3 tokens (3-grams) to find windows containing misspelled tokens that need to be corrected
    tokens.sliding(3).withFilter(_.exists(_.isMisspelled)).foreach {
      case window@Seq(token1, token2, token3) =>
        logger.debug("window: {} {} {}", token1, token2, token3)

        // find candidates for the first token in the window, if misspelled
        val ngram1 = if (token1.isMisspelled) {
          logger.debug("Finding candidate replacements for {}", token1)
          replacementCache.getOrElseUpdate(token1.cleanedText, {
            token1.replacements.map(_.toLowerCase).toSet
          })
        } else Set(token1.cleanedText)

        // find candidates for the second token in the window, if misspelled
        val ngram2 = if (token2.isMisspelled) {
          logger.debug("Finding candidate replacements for {}", token2)
          replacementCache.getOrElseUpdate(token2.cleanedText, {
            token2.replacements.map(_.toLowerCase).toSet
          })
        } else Set(token2.cleanedText)

        // find candidates for the third token in the window, if misspelled
        val ngram3 = if (token3.isMisspelled) {
          logger.debug("Finding candidate replacements for {}", token3)
          replacementCache.getOrElseUpdate(token3.cleanedText, {
            token3.replacements.map(_.toLowerCase).toSet
          })
        } else Set(token3.cleanedText)

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
}

package edu.illinois.i3.emop.apps.pagecorrector

/**
 * Class representing a hyphenated token
 *
 * @param firstToken The first token (before the hyphen)
 * @param secondToken The second token (after the hyphen)
 * @param maxTransformCount
 */
abstract class HyphenatedToken(val firstToken: HOCRToken, val secondToken: HOCRToken, override val maxTransformCount: Int)
  extends HOCRToken(
    s"${firstToken.id}::${secondToken.id}",
    firstToken.text.init concat secondToken.text,
    Math.max(firstToken.noiseConf, secondToken.noiseConf),
    maxTransformCount
  )

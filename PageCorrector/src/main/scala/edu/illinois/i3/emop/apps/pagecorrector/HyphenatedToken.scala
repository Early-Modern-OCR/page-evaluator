package edu.illinois.i3.emop.apps.pagecorrector

abstract class HyphenatedToken(val firstToken: HOCRToken, val secondToken: HOCRToken)
  extends HOCRToken(
    s"${firstToken.id}::${secondToken.id}",
    firstToken.text.init concat secondToken.text,
    Math.max(firstToken.noiseConf, secondToken.noiseConf)
  )

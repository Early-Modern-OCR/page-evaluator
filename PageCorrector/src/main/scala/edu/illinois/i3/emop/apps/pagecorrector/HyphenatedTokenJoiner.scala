package edu.illinois.i3.emop.apps.pagecorrector

import edu.illinois.i3.emop.apps.pagecorrector.PageParser._
import scala.annotation.tailrec

/**
 * Trait for joining hyphenated tokens
 */
trait HyphenatedTokenJoiner {
  type TokenType <: Token
  type HyphenatedTokenType <: TokenType

  protected def createHyphenatedToken(firstToken: TokenType, secondToken: TokenType): HyphenatedTokenType

  def joinHyphenatedTokens(lines: Seq[Line[TokenType]]): Seq[Line[TokenType]] = {
    @tailrec
    def join(acc: Seq[Line[TokenType]], lines: Seq[Line[TokenType]]): Seq[Line[TokenType]] = lines match {
      case l1 :: l2 :: xs if l1.nonEmpty && l2.nonEmpty && l1.last.text.endsWith("-") =>
        join(acc :+ (l1.init :+ createHyphenatedToken(l1.last, l2.head)), if (l2.tail.nonEmpty) l2.tail :: xs else xs)
      case l1 :: xs => join(acc :+ l1, xs)
      case Nil => acc
    }

    join(Seq(), lines)
  }
}

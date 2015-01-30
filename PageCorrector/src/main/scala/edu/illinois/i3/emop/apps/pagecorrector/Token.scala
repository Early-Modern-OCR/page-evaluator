package edu.illinois.i3.emop.apps.pagecorrector

object Token {
  def unapply(token: Token) = Some((token.id, token.text))
}

/**
 * Generic token trait
 */
trait Token {
  def id: String
  def text: String

  lazy val trimmed = text.trim

  override def equals(other: Any) = other match {
    case that: Token => this.id equals that.id
    case _ => false
  }

  override def hashCode() = id.hashCode()

  override def toString =
    s"${this.getClass.getSimpleName}(id: $id, text: '$text')"
}
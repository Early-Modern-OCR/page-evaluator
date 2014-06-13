package edu.illinois.i3.emop.apps.pagecorrector

trait SpellChecker {
  // README: http://norvig.com/spell-correct.html

  import edu.illinois.i3.spellcheck.engine.{Word, SpellDictionary}
  import scala.collection.JavaConversions._

  val dictionaries: Iterable[SpellDictionary]

  def isCorrect(text: String) = dictionaries.exists(_.isCorrect(text.toLowerCase))

  def getSuggestions(text: String, maxDistance: Int = 300): Set[Word] =
    dictionaries.flatMap(_.getSuggestions(text, maxDistance)).toSet
}

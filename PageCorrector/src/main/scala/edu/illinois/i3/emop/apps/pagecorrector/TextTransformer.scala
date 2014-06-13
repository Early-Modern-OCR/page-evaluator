package edu.illinois.i3.emop.apps.pagecorrector

object TextTransformer {
  type TransformRules = Map[String, Set[String]]

  case class Transformation(original: String, replacement: String, index: Int) {
    private val extent = index + original.length
    def overlaps(other: Transformation) = index < other.extent && other.index < extent
  }

  case class TransformedText(text: String, original: String, transformations: Seq[Transformation])
}

trait TextTransformer {
  import TextTransformer._
  import edu.illinois.i3.scala.utils.implicits.CollectionsImplicits._

  val transformRules: TransformRules

  /**
   * Returns the set of all possible transformations that can be applied to the
   * given text based on the supplied rules
   *
   * @param text The input text
   * @return The set of transformation rules that are applicable to the given input text
   */
  protected def getPossibleTransformations(text: String) =
    for {
      index <- 0 until text.length
      (ocrErr, replacements) <- transformRules if text.startsWith(ocrErr, index)
      replacement <- replacements
    } yield Transformation(ocrErr, replacement, index)

  /**
   * Checks whether a set of transformations can be applied together or not
   *
   * @param transforms The set of transformations to check
   * @return True if there is overlap in the transformations (e.g. they cannot be applied together), False otherwise
   */
  protected def hasOverlappedTransformations(transforms: Seq[Transformation]) =
    transforms.sliding(2).exists {
      case Seq(t1, t2) => t2 overlaps t1
      case _ => false
    }

  /**
   * Adjusts the index of each transformation from a group of transformations
   * based on the changes made by applying the transformations in sequence.
   * This is needed before the group of transformations can properly be applied to the same text.
   *
   * @param transforms The group of transformations
   * @return The index-adjusted group of transformations
   */
  protected def adjustTransformationIndexes(transforms: Seq[Transformation]) = {
    var adjust = 0
    transforms.map {
      case t @ Transformation(original, replacement, index) =>
        val newIndex = index + adjust
        adjust += replacement.length - original.length
        if (newIndex == index) t else t.copy(index = newIndex)
    }
  }

  /**
   * Applies a group of transformations to a text and returns the result
   *
   * @param text The text to transform
   * @param transforms The group of (index-adjusted) transformations to apply
   * @return The transformed text
   */
  protected def transform(text: String, transforms: Seq[Transformation]) =
    TransformedText(
      transforms.foldLeft(text) {
        case (s, Transformation(original, replacement, index)) =>
          s.take(index) concat replacement concat s.substring(index + original.length)
      },
      text,
      transforms
    )

  /**
   * Generates all the possible text transforms of an original text given the set of transformation rules supplied
   *
   * @param text The text to transform
   * @param maxTransformCount The maximum number of transformations allowed (puts an upper bound on processing time)
   * @return All possible transformations of the given text
   */
  def transformations(text: String, maxTransformCount: Int = 19) =
    getPossibleTransformations(text)
      .take(maxTransformCount)
      .powerSet
      .withFilter(t => t.nonEmpty && !hasOverlappedTransformations(t))
      .map(adjustTransformationIndexes)
      .map(transform(text, _))
}
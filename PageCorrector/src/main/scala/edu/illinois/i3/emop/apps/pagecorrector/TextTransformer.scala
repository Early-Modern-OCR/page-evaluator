package edu.illinois.i3.emop.apps.pagecorrector

object TextTransformer {
//  type TransformRules = Map[String, Set[String]]
  type TransformRules = List[(String, String)]

  case class Transformation(original: String, replacement: String, index: Int) {
    private val extent = index + original.length
    def overlaps(other: Transformation) = index < other.extent && other.index < extent
  }

  case class TransformedText(text: String, original: String, transformations: Iterable[Transformation])
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
  protected def getPossibleTransformations(text: String) = {
//    for {
//      index <- 0 until text.length
//      (ocrErr, replacements) <- transformRules if text.startsWith(ocrErr, index)
//      replacement <- replacements
//    } yield Transformation(ocrErr, replacement, index)

    for {
      (ocrErr, replacement) <- transformRules
      index <- 0 until text.length if text.startsWith(ocrErr, index)
    } yield Transformation(ocrErr, replacement, index)
  }

  /**
   * Checks whether a set of transformations can be applied together or not
   *
   * @param transforms The set of transformations to check
   * @return True if there is overlap in the transformations (e.g. they cannot be applied together), False otherwise
   */
  protected def hasOverlappedTransformations(transforms: Iterable[Transformation]) =
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
  protected def adjustTransformationIndexes(transforms: Iterable[Transformation]) = {
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
  protected def transform(text: String, transforms: Iterable[Transformation]) =
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
  def transformations(text: String, maxTransformCount: Int = 29): Iterable[TransformedText] = {
    getPossibleTransformations(text)
      .take(maxTransformCount)
      .sortBy(_.index)
      .powerSetWithExclusiveFilter(!hasOverlappedTransformations(_))
      .map(adjustTransformationIndexes)
      .map(transform(text, _))
  }

  /**
   * Finds all the longest (start,end) pairs of indices into the given text delimiting transformable text
   *
   * @param text The text
   * @param nonTransformableCharTolerance How many non-transformable characters to allow to be included in the (start,end) sequences found (default = 0)
   * @return The longest (start,end) pairs of indices into the given text delimiting transformable text
   */
  def findTransformableParts(text: String, nonTransformableCharTolerance: Int = 0) = {
    //val ruleCover = transformRules.keys.withFilter(text.contains).flatMap(err =>
    val ruleCover = transformRules.map { case (ocrErr, _) => ocrErr }.withFilter(text.contains).flatMap(err =>
      (0 until text.length).collect { case i if text.substring(i).startsWith(err) => (i, i+err.length) })
    val charCover = text.zipWithIndex.collect { case (c, i) if c.isLetter => (i, i+1) }
    val cover = Set(charCover ++ ruleCover: _*)
    var parts = cover

    while (parts.exists(x => cover.exists(y => x._1 == y._2 || x._2 == y._1))) {
      parts = parts.flatMap {
        case x @ (a, b) =>
          val res = cover.collect {
            case (c, d) if a == d => (c, b)
            case (c, d) if b == c => (a, d)
          }
          if (res.isEmpty) Set(x) else res
      }
    }

    import edu.illinois.i3.scala.utils.collections.::>
    import Math.{min,max}

    parts.toSeq.sorted.foldLeft(List.empty[List[(Int,Int)]])((acc, x) => acc match {
      case Nil => List(List(x))
      case init ::> last if x._1 - last.last._2 <= nonTransformableCharTolerance => init :+ (last match {
        case i ::> l if x._1 > l._2 => last :+ x
        case i ::> l => i :+ (min(l._1, x._1), max(l._2, x._2))
      })
      case lst => lst :+ List(x)
    })
  }
}
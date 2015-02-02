package edu.illinois.i3.emop.apps.pagecorrector
import java.util.regex.Pattern

object Regexps {
  val AlphaPattern = Pattern.compile("""\p{L}""", Pattern.CANON_EQ)
  val AllAlphaPattern = Pattern.compile("""^\p{L}+$""", Pattern.CANON_EQ)
  val Repeated4orMoreCharsPattern = Pattern.compile("""(\P{N})\1{3,}""", Pattern.CANON_EQ)
  val BeginPunctPattern = """^[^\p{L}\p{N}]*""".r
  val EndPunctPattern = """[^\p{L}\p{N}]*$""".r
}

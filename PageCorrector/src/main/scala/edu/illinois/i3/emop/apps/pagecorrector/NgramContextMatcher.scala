package edu.illinois.i3.emop.apps.pagecorrector

import com.jolbox.bonecp.BoneCP

object NgramContextMatcher {
  protected val THREEGRAM_TABLE = "ngram3_lower"
  protected val NGRAM_KEY_TABLE = "ngram_key"

  protected val SQL_3GRAM_BEST_MATCH =
    s"""
      | SELECT
      |   n1.ngram AS ngram1, n2.ngram AS ngram2, n3.ngram AS ngram3,
      |   SUM(g3.match_count) AS total_match_count, SUM(g3.vol_count) AS total_vol_count
      | FROM
      |   $THREEGRAM_TABLE g3
      |   JOIN $NGRAM_KEY_TABLE n1 ON g3.ngram1_id=n1.ngram_id
      |   JOIN $NGRAM_KEY_TABLE n2 ON g3.ngram2_id=n2.ngram_id
      |   JOIN $NGRAM_KEY_TABLE n3 ON g3.ngram3_id=n3.ngram_id
      | WHERE
      |   n1.ngram IN (%s) AND n2.ngram IN (%s) AND n3.ngram IN (%s)
      | GROUP BY
      |   n1.ngram, n2.ngram, n3.ngram
      | %s
      | ORDER BY 
      |   total_match_count DESC, 
      |   total_vol_count DESC 
      | LIMIT 1
    """.stripMargin

  protected val SQL_3GRAM_MATCHES =
    s"""
      | SELECT
      |   n1.ngram AS ngram1, n2.ngram AS ngram2, n3.ngram AS ngram3,
      |   SUM(g3.match_count) AS total_match_count, SUM(g3.vol_count) AS total_vol_count
      | FROM
      |   $THREEGRAM_TABLE g3
      |   JOIN $NGRAM_KEY_TABLE n1 ON g3.ngram1_id=n1.ngram_id
      |   JOIN $NGRAM_KEY_TABLE n2 ON g3.ngram2_id=n2.ngram_id
      |   JOIN $NGRAM_KEY_TABLE n3 ON g3.ngram3_id=n3.ngram_id
      | WHERE
      |   n1.ngram IN (%s) AND n2.ngram IN (%s) AND n3.ngram IN (%s)
      | GROUP BY
      |   n1.ngram, n2.ngram, n3.ngram
      | %s;
    """.stripMargin
}

class NgramContextMatcher(connPool: BoneCP, minContextMatchCount: Option[Int] = None, minContextVolCount: Option[Int] = None) {
  import NgramContextMatcher._
  import edu.illinois.i3.scala.utils.implicits.SqlImplicits._
  import edu.illinois.i3.scala.utils.implicits.StringsImplicits._
  import scala.util.Try

  val restrictionClause = (minContextMatchCount, minContextVolCount) match {
    case (Some(minMatchCount), None) => s" HAVING total_match_count >= $minMatchCount"
    case (None, Some(minVolCount)) => s" HAVING total_vol_count >= $minVolCount"
    case (Some(minMatchCount), Some(minVolCount)) => s" HAVING total_match_count >= $minMatchCount AND total_vol_count >= $minVolCount"
    case _ => ""
  }

  def bestMatch(ngram1: Iterable[String], ngram2: Iterable[String], ngram3: Iterable[String]) = Try {
    val connection = connPool.getConnection
    val stmt = connection.createStatement()
    try {
      val query = SQL_3GRAM_BEST_MATCH.format(
        ngram1.map(_.toLowerCase.quoted()).mkString(","),
        ngram2.map(_.toLowerCase.quoted()).mkString(","),
        ngram3.map(_.toLowerCase.quoted()).mkString(","),
        restrictionClause
      )

      val resultSet = stmt.executeQuery(query)
      if (resultSet.next())
        Some(ContextMatch(
          resultSet.getString(1), // ngram1
          resultSet.getString(2), // ngram2
          resultSet.getString(3), // ngram3
          resultSet.getInt(4),    // total_match_count
          resultSet.getInt(5)     // total_vol_count
        ))
      else
        None
    }
    finally {
      connection.tryRelease(stmt)
    }
  }

  def matches(ngram1: Iterable[String], ngram2: Iterable[String], ngram3: Iterable[String]) = Try {
    val connection = connPool.getConnection
    val stmt = connection.createStatement()
    try {
      val query = SQL_3GRAM_MATCHES.format(
        ngram1.map(_.toLowerCase.quoted()).mkString(","),
        ngram2.map(_.toLowerCase.quoted()).mkString(","),
        ngram3.map(_.toLowerCase.quoted()).mkString(","),
        restrictionClause
      )

      val resultSet = stmt.executeQuery(query)
      val matches = new Iterator[ContextMatch] {
        override def hasNext = resultSet.next()
        override def next() = ContextMatch(
          resultSet.getString(1), // ngram1
          resultSet.getString(2), // ngram2
          resultSet.getString(3), // ngram3
          resultSet.getInt(4),    // total_match_count
          resultSet.getInt(5)     // total_vol_count
        )
      }

      matches.toIndexedSeq
    }
    finally {
      connection.tryRelease(stmt)
    }
  }
}

case class ContextMatch(ngram1: String, ngram2: String, ngram3: String, matchCount: Int, volCount: Int)
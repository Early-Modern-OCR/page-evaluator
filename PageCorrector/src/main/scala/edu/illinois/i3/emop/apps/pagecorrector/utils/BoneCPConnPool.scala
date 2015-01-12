package edu.illinois.i3.emop.apps.pagecorrector.utils

import com.jolbox.bonecp.{UsernamePassword, BoneCPConfig, BoneCP}
import scala.util.Try
import scala.util.Success
import scala.util.Failure

abstract class BoneCPConnPool {
  val BONECP_MIN_CONN_PER_PART: Int
  val BONECP_MAX_CONN_PER_PART: Int
  val BONECP_PARTITION_COUNT: Int

  def createConnectionPool(dbDriver: String, dbUrl: String, dbCreds: Option[UsernamePassword]): Try[BoneCP] = Try {
    Class.forName(dbDriver)

    val config = new BoneCPConfig()
    config.setJdbcUrl(dbUrl)
    config.setMinConnectionsPerPartition(BONECP_MIN_CONN_PER_PART)
    config.setMaxConnectionsPerPartition(BONECP_MAX_CONN_PER_PART)
    config.setPartitionCount(BONECP_PARTITION_COUNT)

    dbCreds match {
      case Some(creds) =>
        config.setUsername(creds.getUsername)
        config.setPassword(creds.getPassword)
      case None =>
    }

    new BoneCP(config)
  }
}
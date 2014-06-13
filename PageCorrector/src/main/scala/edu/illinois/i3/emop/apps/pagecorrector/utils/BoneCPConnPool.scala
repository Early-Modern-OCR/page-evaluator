package edu.illinois.i3.emop.apps.pagecorrector.utils

import com.jolbox.bonecp.BoneCPConfig
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import com.jolbox.bonecp.BoneCP

abstract class BoneCPConnPool {
  val BONECP_MIN_CONN_PER_PART: Int
  val BONECP_MAX_CONN_PER_PART: Int
  val BONECP_PARTITION_COUNT: Int

  def createConnectionPool(dbDriver: String, dbUrl: String, dbUser: String, dbPasswd: String): Try[BoneCP] = Try {
    Class.forName(dbDriver)

    val config = new BoneCPConfig()
    config.setJdbcUrl(dbUrl)
    config.setMinConnectionsPerPartition(BONECP_MIN_CONN_PER_PART)
    config.setMaxConnectionsPerPartition(BONECP_MAX_CONN_PER_PART)
    config.setPartitionCount(BONECP_PARTITION_COUNT)
    config.setUsername(dbUser)
    config.setPassword(dbPasswd)

    new BoneCP(config)
  }
}
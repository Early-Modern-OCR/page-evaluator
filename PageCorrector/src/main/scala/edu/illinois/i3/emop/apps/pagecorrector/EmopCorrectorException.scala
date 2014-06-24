package edu.illinois.i3.emop.apps.pagecorrector

case class EmopCorrectorException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

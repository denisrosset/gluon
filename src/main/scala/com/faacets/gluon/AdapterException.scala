package com.faacets.gluon

/**
  * Created by denis on 02.04.17.
  */
class AdapterException(message: String, cause: Throwable) extends IllegalArgumentException(message, cause) {

  def this(message: String) = this(message, null)

  def this() = this(null, null)

  def this(cause: Throwable) = this(null, cause)

}

package com.faacets.gluon

/** Exception type to throw when the conversion is unsuccesful; is wrapped by the adapter
  * in a [[ArgAdapterException]] or [[ResAdapterException]]
  *
  * @param message   Error message describing the issue
  *
  * @param cause     Optional wrapped exception or null
  */
class AdapterException(message: String, cause: Throwable) extends IllegalArgumentException(message, cause) {

  def this(message: String) = this(message, null)

  def this() = this(null, null)

  def this(cause: Throwable) = this(null, cause)

}

package com.faacets.gluon

import java.math.{BigDecimal, BigInteger}

/** Additional conversion methods for the standard numerical types. */
object NumUtils {

  implicit class richBigInteger(val bi: BigInteger) extends AnyVal {
    def toBigInt: BigInt = BigInt(bi)
    def isValidDouble: Boolean = (toDouble.toBigInteger == bi)
    def toDouble: Double = new BigDecimal(bi).doubleValue
    def isValidInt: Boolean = BigInt(bi).isValidInt
    def toInt: Int = BigInt(bi).toInt
    def isValidLong: Boolean = BigInt(bi).isValidLong
    def toLong: Long = BigInt(bi).toLong
  }

  implicit class richDouble(val d: Double) extends AnyVal {
    def toBigInt: BigInt = BigInt(toBigInteger)
    def toBigInteger: BigInteger = new BigDecimal(d).toBigInteger
  }

  implicit class richLong(val l: Long) extends AnyVal {
    def isValidDouble: Boolean = l.toDouble.toLong == l
  }

}

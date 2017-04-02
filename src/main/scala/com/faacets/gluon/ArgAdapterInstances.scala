package com.faacets.gluon

import java.math.BigInteger

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

import NumUtils._

trait ArgAdapterInstances {

  implicit val double: ArgAdapter[Double] = ArgAdapter[Double]("Double") {
    case b: Byte => b.toDouble
    case s: Short => s.toDouble
    case c: Char => c.toDouble
    case i: Int => i.toDouble
    case l: Long if l.isValidDouble => l.toDouble
    case l: Long => throw new AdapterException("Cannot be represented exactly as Double")
    case bi: BigInteger if bi.isValidDouble => bi.toDouble
    case bi: BigInteger => throw new AdapterException("Cannot be represented exactly as Double")
    case f: Float => f.toDouble
    case d: Double => d.toDouble
    case s: String => try { s.toDouble } catch { case e: Exception => throw new AdapterException(e) }
    case a => throw new AdapterException()
  }

  implicit val int: ArgAdapter[Int] = ArgAdapter[Int]("Int") {
    case b: Byte => b.toInt
    case s: Short => s.toInt
    case c: Char => c.toInt
    case i: Int => i.toInt
    case bi: BigInteger if bi.isValidInt => bi.toInt
    case l: Long if l.isValidInt => l.toInt
    case f: Float if f.isValidInt => f.toInt
    case d: Double if d.isValidInt => d.toInt
    case s: String => try { s.toInt } catch { case e: Exception => throw new AdapterException(e) }
    case _: Long | _: Float | _: Double => throw new AdapterException("Out of range")
    case a => throw new AdapterException()
  }

  implicit val bigInt: ArgAdapter[BigInt] = ArgAdapter[BigInt]("BigInt") {
    case b: Byte => BigInt(b)
    case s: Short => BigInt(s)
    case c: Char => BigInt(c)
    case i: Int => i.toBigInt
    case bi: java.math.BigInteger => BigInt(bi)
    case l: Long => l.toBigInt
    case f: Float if f.isWhole => f.toBigInt
    case d: Double if d.isWhole => d.toBigInt
    case _: Float | _: Double => throw new AdapterException("Not an integer")
    case a => throw new AdapterException()
  }

  implicit def seq[S[X] <: Seq[X], A:ArgAdapter](implicit cbf: CanBuildFrom[Seq[A], A, S[A]]): ArgAdapter[S[A]] =
    ArgAdapter[S[A]]("Seq["+ArgAdapter[A].name+"]") {
      case array: Array[_] => array.map(ArgAdapter[A].to(_)).toSeq.to[S]
      case list: java.util.List[_] =>
        import scala.collection.JavaConverters._
        list.asScala.map(ArgAdapter[A].to(_)).to[S]
      case _ => throw new AdapterException()
    }

}

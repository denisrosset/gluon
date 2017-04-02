package com.faacets.gluon

import java.math.BigInteger

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

class ArgAdapterException(a: ArgAdapter[_], argIndex: Int, argName: String, arg: Any, cause: AdapterException) extends IllegalArgumentException(s"Argument $argName (#$argIndex): cannot adapt to ${a.name} the given $arg of class ${arg.getClass.getName}", cause)

class ResAdapterException(a: ResAdapter[_, _], res: Any, cause: AdapterException) extends IllegalArgumentException(s"Result cannot be adapted to ${a.name} from $res of class ${res.getClass.getName}", cause)

class AdapterException(message: String, cause: Throwable) extends IllegalArgumentException(message, cause) {

  def this(message: String) = this(message, null)

  def this() = this(null, null)

  def this(cause: Throwable) = this(null, cause)

}

object ArgAdapter {

  def apply[A](implicit ev: ArgAdapter[A]): ArgAdapter[A] = ev

  def apply[A](name: String)(f: Any => A): ArgAdapter[A] = new ArgAdapter[A](name, f)

  import java.math.{BigDecimal, BigInteger}

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

  implicit val double: ArgAdapter[Double] = apply[Double]("Double") {
    case b: Byte => b.toDouble
    case s: Short => s.toDouble
    case c: Char => c.toDouble
    case i: Int => i.toDouble
    case l: Long if l.isValidDouble => throw new AdapterException("Cannot be represented exactly as Double")
    case l: Long => l.toDouble
    case bi: BigInteger if bi.isValidDouble => bi.toDouble
    case f: Float => f.toDouble
    case d: Double => d.toDouble
    case s: String => try { s.toDouble } catch { case e: Exception => throw new AdapterException(e) }
    case a => throw new AdapterException()
  }

  implicit val int: ArgAdapter[Int] = apply[Int]("Int") {
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

  implicit val bigInt: ArgAdapter[BigInt] = apply[BigInt]("BigInt") {
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

  import scala.collection.generic.CanBuildFrom
  import scala.language.higherKinds

  implicit def seq[S[X] <: Seq[X], A:ArgAdapter](implicit cbf: CanBuildFrom[Seq[A], A, S[A]]): ArgAdapter[S[A]] = apply[S[A]]("Seq["+ArgAdapter[A].name+"]") {
    case array: Array[_] => array.map(ArgAdapter[A].to(_)).toSeq.to[S]
    case list: java.util.List[_] =>
      import scala.collection.JavaConverters._
      list.asScala.map(ArgAdapter[A].to(_)).to[S]
    case _ => throw new AdapterException()
  }

}

class ArgAdapter[+A](val name: String, f: Any => A) {

  def argTo(argIndex: Int, argName: String, arg: Any): A = try {
    f(arg)
  } catch {
    case ae: AdapterException => throw new ArgAdapterException(this, argIndex, argName, arg, ae)
  }

  def to(arg: Any): A = f(arg)

}

class ResAdapter[-A, +R](val name: String, f: A => R) {

  def resFrom(res: A): R = try {
    f(res)
  } catch {
    case ae: AdapterException => throw new ResAdapterException(this, res, ae)
  }

  def from(res: A): R = f(res)

}

object ResAdapter {

  def apply[A, R](implicit ev: ResAdapter[A, R]): ResAdapter[A, R] = ev

  def apply[A, R](name: String)(f: A => R): ResAdapter[A, R] = new ResAdapter[A, R](name, f)

  implicit def seq[A, B:ClassTag](implicit ev: ResAdapter[A, B]): ResAdapter[Seq[A], Array[B]] = apply[Seq[A], Array[B]]("Seq["+ResAdapter[A, B].name+"]") { seq =>
    seq.map(a => ResAdapter[A, B].from(a)).toArray
  }

  implicit val long: ResAdapter[Long, Long] = apply("Long")(identity)

  implicit val int: ResAdapter[Int, Int] = apply("Int")(identity)

  implicit val double: ResAdapter[Double, Double] = apply("Double")(identity)

  implicit val bigInt: ResAdapter[BigInt, BigInteger] = apply("BigInt")(_.bigInteger)

}

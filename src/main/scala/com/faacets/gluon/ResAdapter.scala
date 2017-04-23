package com.faacets.gluon

import java.math.BigInteger

import scala.reflect.ClassTag

/** Exception thrown when adapting the return value of the Scala code, when the result value cannot be represented
  * by the corresponding standard Java type. */
class ResAdapterException(a: ResAdapter[_, _], res: Any, cause: AdapterException)
  extends IllegalArgumentException(s"Result cannot be adapted to ${a.name} from $res of class ${res.getClass.getName}", cause)

/** Adapts the result of the Scala code by transforming the return value into a standard Java type. */
class ResAdapter[-A, +R](val name: String, f: A => R) {

  /** Wraps the conversion of the Scala type A in the standard Java type R
    * in a try/catch block to provide better error reporting. */
  def resFrom(res: A): R = try {
    f(res)
  } catch {
    case ae: AdapterException => throw new ResAdapterException(this, res, ae)
  }

  /** Converts the result in the Scala type A to the standard Java type R. */
  def from(res: A): R = f(res)

  def contramap[B](newName: String)(g: B => A): ResAdapter[B, R] = new ResAdapter[B, R](newName, g andThen f)

}

object ResAdapter extends ResAdapterInstances {

  /** Returns an implicit instance; shorthand for `implicitly[ResAdapter[A, R]]` */
  def apply[A, R](implicit ev: ResAdapter[A, R]): ResAdapter[A, R] = ev

  /** Creates a ResAdapter from the name of the type and a conversion function.
    * The function should throw [[AdapterException]] on failure (this exception type
    * is wrapped with additional information when propagated, which is not the case
    * for other exception types).
    */
  def apply[A, R](name: String)(f: A => R): ResAdapter[A, R] = new ResAdapter[A, R](name, f)

}
